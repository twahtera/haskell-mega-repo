{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import Futurice.Prelude

import Control.Monad.Http   (evalHttpT)
import Control.Monad.Logger (LogLevel (..), LogSource, filterLogger)
import Control.Monad.Reader (runReaderT)
import Data.Maybe           (isJust)
import Data.Time            (UTCTime (..))
import Data.Time.TH         (mkUTCTime)
import System.Environment   (getArgs, lookupEnv)
import System.IO            (hPutStrLn, stderr)

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..), linebreak,
                                                putDoc)

import qualified Data.Vector as V

import PlanMill

main :: IO ()
main = do
    debug <- isJust <$> lookupEnv "DEBUG"
    args <- getArgs
    case args of
        [uid, apikey, endpoint] ->
            integration debug (Ident $ read uid) (ApiKey $ fromString apikey) endpoint
        _                       ->
            hPutStrLn stderr "to run integration tests: ./integration uid apikey endpoint"

putPretty :: (MonadIO m, AnsiPretty a) => a -> m ()
putPretty = liftIO . putDoc . (<> linebreak) . ansiPretty

script1 :: (MonadPlanMill m, MonadIO m) => m ()
script1 = planmillAction me >>= putPretty

script2 :: (MonadPlanMill m, MonadIO m) => m ()
script2 = do
    ps <- planmillVectorAction projects
    let p = V.head ps
    p' <- planmillAction $ project $ p ^. identifier
    putPretty p
    putPretty p'
    putPretty (p == p')
    ts <- planmillVectorAction $ projectTasks $ p ^. identifier
    putPretty ts
    as <- planmillVectorAction $ projectAssignments $ p ^. identifier
    putPretty as

script3 :: (MonadPlanMill m, MonadIO m) => m ()
script3 = do
    us <- planmillVectorAction users
    let us' = V.take 5 us
    putPretty us'
    let u = V.head us
    u' <- planmillAction $ user $ u ^. identifier
    putPretty u
    putPretty u'
    putPretty (u == u')

script4 :: (MonadPlanMill m, MonadIO m) => m ()
script4 = do
    ts <- planmillVectorAction teams
    let ts' = V.take 5 ts
    putPretty ts'
    let t = V.head ts
    t' <- planmillAction $ team $ t ^. identifier
    putPretty t
    putPretty t'
    putPretty (t == t')

script5 :: (MonadPlanMill m, MonadIO m, MonadThrow m) => m ()
script5 = do
    interval <- mkResultInterval IntervalStart
        $(mkUTCTime "2015-01-01T00:00:00.000Z")
        $(mkUTCTime "2016-01-01T00:00:00.000Z")
    interval' <- mkInterval
        (utctDay $(mkUTCTime "2016-01-01T00:00:00.000Z"))
        (utctDay $(mkUTCTime "2016-02-01T00:00:00.000Z"))
    me' <- planmillAction me
    as <- planmillVectorAction $ reportableAssignments (me' ^. identifier)
    tb <- planmillAction $ userTimeBalance (me' ^. identifier)
    trs <- planmillVectorAction $ timereportsFromIntervalFor interval (me' ^. identifier)
    cc <- planmillVectorAction $ userCapacity interval' (me' ^. identifier)
    putPretty as
    putPretty tb
    putPretty trs
    putPretty cc

script6 :: (MonadPlanMill m, MonadIO m, MonadThrow m) => m ()
script6 = do
    me' <- planmillAction me
    u <- planmillAction $ user $ me' ^. identifier
    putPretty u
    c <- enumerationValue (uContractType u) (fromString "Unknown Contract")
    putPretty c
    p <- enumerationValue (uPassive u) (fromString "Unknown passivity")
    putPretty p

integration :: Bool -> UserId -> ApiKey -> String -> IO ()
integration debug uid apikey endpoint = id
    $ evalHttpT
    $ runStderrLoggingT
    $ filterLogger logPredicate
    $ flip runReaderT cfg $ sequence_
        [ script1, script2, script3
        , script4, script5, script6
        ]
  where
    cfg :: Cfg
    cfg = Cfg uid apikey endpoint

    logPredicate :: LogSource -> LogLevel -> Bool
    logPredicate _ level = debug || level > LevelDebug
