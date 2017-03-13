{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main (main) where

import Prelude ()
import Futurice.Prelude
import Control.Lens                (taking)
import Control.Monad.Base          (liftBaseDefault)
import Control.Monad.Http          (MonadHttp (..))
import Control.Monad.Trans.Control (defaultLiftWith, defaultRestoreT)
import Data.Constraint
import Data.TDigest.Metrics        (MonadMetrics)
import Futurice.CryptoRandom       (evalCRandTThrow', mkCryptoGen)
import Futurice.EnvConfig          (getConfig)
import PlanMill.Eval               (evalPlanMill)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty
       (AnsiPretty (..), linebreak, putDoc)

import qualified Data.Aeson              as Aeson
import qualified Data.ByteString.Lazy    as LBS
import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H
import qualified Options.SOP             as O

import qualified PlanMill as PM

import Data.Reflection (reifySymbol)
import GHC.TypeLits    (KnownSymbol)

-------------------------------------------------------------------------------
-- Cmd
-------------------------------------------------------------------------------

data Cmd
    = CmdMe
    | CmdUsers
    | CmdUser PM.UserId
    | CmdTimereports PM.UserId (PM.Interval Day)
    | CmdReportableAssignments PM.UserId
    | CmdTask PM.TaskId
    | CmdMeta Text
    | CmdEnumeration Text
    | CmdProject PM.ProjectId
    | CmdAccounts
    | CmdAccount PM.AccountId

deriveGeneric ''Cmd

-------------------------------------------------------------------------------
-- Cli
-------------------------------------------------------------------------------

data Opts = Opts
    { optsDumpJson :: !Bool
    , optsDumpRaw  :: !Bool
    , optsShowAll  :: !Bool
    }
  deriving Show

optsP :: O.Parser Opts
optsP = Opts
    <$> O.flag False True (O.long "dump-json" <> O.help "Print json")
    <*> O.flag False True (O.long "dump-raw" <> O.help "Print raw reqsponse")
    <*> O.flag False True (O.long "show-all" <> O.help "Show all entries, default: 10")

main :: IO ()
main = withStderrLogger $ \logger -> runLogT "pm-cli" logger $ do
    f <- liftIO $ O.execParser opts
    cfg <- getConfig "PM"
    f cfg
  where
    opts = O.info (O.helper <*> (execute <$> optsP <*> O.sopCommandParser)) $ mconcat
        [ O.fullDesc
        , O.progDesc "Planmill Client"
        , O.header "Let's see what it returns"
        ]

-------------------------------------------------------------------------------
-- Execution
-------------------------------------------------------------------------------

execute :: Opts -> Cmd -> PM.Cfg -> LogT IO ()
execute opts cmd cfg = runPlanmillT cfg opts $ case cmd of
    CmdMe -> do
        x <- PM.planmillAction PM.me
        putPretty x
    CmdUsers -> do
        x <- PM.planmillAction PM.users
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdUser uid -> do
        x <- PM.planmillAction $ PM.user uid
        putPretty x
    CmdAccounts -> do
        x <- PM.planmillAction PM.accounts
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdAccount aid -> do
        x <- PM.planmillAction $ PM.account aid
        putPretty x
    CmdTimereports uid interval -> do
        x <- PM.planmillAction $ PM.timereportsFromIntervalFor
            (PM.ResultInterval PM.IntervalStart interval)
            uid
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdReportableAssignments uid -> do
        x <- PM.planmillAction $ PM.reportableAssignments uid
        putPretty $ if optsShowAll opts
            then x ^.. folded
            else x ^.. taking 10 traverse
    CmdTask tid -> do
        x <- PM.planmillAction $ PM.task tid
        putPretty x
    CmdMeta path  -> do
        x <- PM.planmillAction $ PM.planMillGet path
        putPretty (x :: PM.Meta)
    CmdProject pid -> do
        x <- PM.planmillAction $ PM.project pid
        putPretty x
    CmdEnumeration enum -> reifyTextSymbol enum $ \p -> do
        x <- PM.planmillAction $ PM.enumerations p
        putPretty x

-------------------------------------------------------------------------------
-- PlanmillT: TODO move to planmill-client
-------------------------------------------------------------------------------

-- TODO: this abit of waste as it reinitialises crypto for each request
newtype PlanmillT m a = PlanmillT { runPlanmillT' :: ReaderT (PM.Cfg, Opts) m a }

runPlanmillT :: PM.Cfg -> Opts -> PlanmillT m a -> m a
runPlanmillT cfg opts m = runReaderT (runPlanmillT' m) (cfg, opts)

instance Functor m => Functor (PlanmillT m) where
    fmap f (PlanmillT x) = PlanmillT $ fmap f x
instance Applicative m => Applicative (PlanmillT m) where
    pure = PlanmillT . pure
    PlanmillT f <*> PlanmillT x = PlanmillT (f <*> x)
instance Monad m => Monad (PlanmillT m) where
    return = pure
    m >>= k = PlanmillT $ runPlanmillT' m >>= runPlanmillT' . k

instance MonadTrans PlanmillT where
    lift = PlanmillT . lift

instance MonadError e m => MonadError e (PlanmillT m) where
    throwError = lift . throwError
    catchError m h = PlanmillT $ runPlanmillT' m `catchError` (runPlanmillT' . h)

instance MonadIO m => MonadIO (PlanmillT m) where
    liftIO = lift . liftIO

instance MonadTransControl PlanmillT where
    type StT PlanmillT a = StT (ReaderT PM.Cfg) a
    liftWith = defaultLiftWith PlanmillT runPlanmillT'
    restoreT = defaultRestoreT PlanmillT

instance Monad m => PM.MonadPlanMillConstraint (PlanmillT m) where
    type MonadPlanMillC (PlanmillT m) = Aeson.FromJSON
    entailMonadPlanMillCVector _ _ = Sub Dict

instance
    (MonadIO m, MonadClock m, MonadLog m, MonadThrow m, MonadMetrics m)
    => PM.MonadPlanMill (PlanmillT m)
  where
    planmillAction planmill = PlanmillT $ ReaderT $ \(cfg, opts) -> do
        g <- mkCryptoGen
        evalHttpT opts $
            flip runReaderT cfg $
            evalCRandTThrow' g $
            evalPlanMill planmill

-------------------------------------------------------------------------------
-- HttpT
-------------------------------------------------------------------------------

newtype HttpT m a = HttpT { runHttpT :: ReaderT (H.Manager, Opts) m a }
  deriving (Functor, Applicative, Monad, MonadThrow, MonadTime)

evalHttpT :: MonadIO m => Opts -> HttpT m a -> m a
evalHttpT opts m = do
    mgr <- liftIO (H.newManager H.tlsManagerSettings)
    runReaderT (runHttpT m) (mgr, opts)

instance MonadTrans HttpT where
    lift = HttpT . lift

instance MonadBase b m => MonadBase b (HttpT m) where
    liftBase = liftBaseDefault

instance MonadIO m => MonadIO (HttpT m) where
    liftIO = HttpT . liftIO

instance MonadIO m => MonadHttp (HttpT m) where
    httpLbs req = HttpT $ do
        (mgr, opts) <- ask
        res <- liftIO $ H.httpLbs req mgr
        when (optsDumpRaw opts) $ liftIO $ do
            LBS.putStr (H.responseBody res)
            putChar '\n'
        when (optsDumpJson opts) $
            for_ (Aeson.decode (H.responseBody res) :: Maybe Value) putPretty
        pure res

instance MonadTransControl HttpT where
    type StT HttpT a = a
    liftWith = defaultLiftWith HttpT runHttpT
    restoreT = defaultRestoreT HttpT

-------------------------------------------------------------------------------
-- putPretty
-------------------------------------------------------------------------------

putPretty :: (MonadIO m, AnsiPretty a) => a -> m ()
putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty

-------------------------------------------------------------------------------
-- move to futurice-prelude
-------------------------------------------------------------------------------

reifyTextSymbol :: forall r. Text -> (forall n. KnownSymbol n => Proxy n -> r) -> r
reifyTextSymbol t = reifySymbol (t ^. unpacked)
