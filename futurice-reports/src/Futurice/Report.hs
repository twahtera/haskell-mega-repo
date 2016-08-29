{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | The report i.e. fancy table.
module Futurice.Report (
    -- * Reports
    Report(..),
    ReportHeader(..),
    ReportRow(..),
    ReportCsvRow(..),
    overReportRow,
    -- ** Typeclasses to generate reports
    IsReport(..),
    defaultReportExec,
    readerReportExec,
    ToReportRow(..),
    -- * Useful helpers
    Per(..),
    ReportGenerated(..),
    ) where

import Futurice.Peano
import Futurice.Prelude

import Control.Monad.Trans.Identity (IdentityT (..))
import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Constraint              (Constraint)
import Data.FileEmbed               (embedStringFile)
import Data.Functor.Identity        (Identity (..))
import Data.Swagger                 (ToSchema (..))
import Data.These                   (These (..))
import GHC.TypeLits                 (KnownSymbol, Symbol, symbolVal)
import Lucid                        hiding (for_)
import Lucid.Base                   (HtmlT (..))
import Lucid.Foundation.Futurice    (defPageParams, large_, pageJs, page_, row_)

import Servant.API         (MimeRender (..))
import Servant.CSV.Cassava (CSV', EncodeOpts (..))

import qualified Data.Csv    as Csv
import qualified Data.Set    as Set
import qualified Data.Text   as T
import qualified Futurice.IC as IList

-------------------------------------------------------------------------------
-- ReportGenerated
-------------------------------------------------------------------------------

-- | Report generation time.
newtype ReportGenerated = ReportGenerated { getReportGenerated :: UTCTime }
    deriving (Typeable)

instance ToJSON ReportGenerated where
    toJSON = toJSON . getReportGenerated

instance FromJSON ReportGenerated  where
    parseJSON = fmap ReportGenerated . parseJSON

instance ToHtml ReportGenerated where
    toHtml r = do
      "Generated at "
      toHtml . show . getReportGenerated $ r
    toHtmlRaw = toHtml

-------------------------------------------------------------------------------
-- Per
-------------------------------------------------------------------------------

-- | Strict pair which is useful for adding layers to reports.
-- Results into cartesian product in the report.
--
-- @
-- type Rep = Report
--     "Example report"
--     '[Vector, Per Something, Vector]
--     SomeVal
-- @
data Per a b = Per
    { perFst :: !a
    , perSnd :: !b
    }
    deriving (Eq, Show, Functor, Foldable, Traversable, Typeable)

instance (ToJSON a, ToJSON b) => ToJSON (Per a b) where
    toJSON (Per a b) = toJSON (a, b)

instance (FromJSON a, FromJSON b) => FromJSON (Per a b) where
    parseJSON = fmap (uncurry Per) . parseJSON

instance (ToReportRow a, ToReportRow b) => ToReportRow (Per a b) where
    type ReportRowLen (Per a b) = PAdd (ReportRowLen a) (ReportRowLen b)

    type ReportRowC (Per a b) m = (ReportRowC a m, ReportRowC b m)

    reportHeader _ =
        let ReportHeader a = reportHeader (Proxy :: Proxy a)
            ReportHeader b = reportHeader (Proxy :: Proxy b)
        in ReportHeader (IList.append a b)

    reportRow (Per a b) = do
        ReportRow cls  row  <- reportRow a
        ReportRow cls' row' <- reportRow b
        pure $ ReportRow (cls <> cls') (IList.append row row')

    reportCsvRow (Per a b) = do
        ReportCsvRow  row  <- reportCsvRow a
        ReportCsvRow  row' <- reportCsvRow b
        pure $ ReportCsvRow (IList.append row row')

-------------------------------------------------------------------------------
-- Lucid
-------------------------------------------------------------------------------

-- | Header of the report.
newtype ReportHeader (n :: Peano) = ReportHeader
    { getReportHeader :: IList.IList n Text }
    deriving (Typeable)

-- | Single row of the report.
data ReportRow m (n :: Peano) = ReportRow
    { getReportRowCls :: Set Text
    , getReportRow :: !(IList.IList n (HtmlT m ()))
    }
    deriving (Typeable)

-- | Single row of the CSV report.
newtype ReportCsvRow m (n :: Peano) = ReportCsvRow
    { getReportCsv :: IList.IList n (IdentityT m Csv.Field)
    }
    deriving (Typeable)

-- | Map over report row contents.
overReportRow
    :: Monad m
    => (IList.IList n (HtmlT m ()) -> IList.IList n' (HtmlT m ()))
    -> ReportRow m n -> ReportRow m n'
overReportRow f (ReportRow cls row) = ReportRow cls . f $ row

-- | Transform type to @['ReportRow']@.
--
-- *TODO:* we could also separate "singleton" and "listy" types.
class ToReportRow a where
    -- | How much columns there is?
    type ReportRowLen a :: Peano

    -- | Additional constraints on @HtmlT m@ monad
    type ReportRowC a (m :: * -> *) :: Constraint
    type ReportRowC a m = ()

    reportHeader :: Proxy a -> ReportHeader (ReportRowLen a)
    reportRow
        :: (Applicative m, Monad m, ReportRowC a m)
        => a -> [ReportRow m (ReportRowLen a)]

    reportCsvRow
        :: (Applicative m, Monad m, ReportRowC a m)
        => a -> [ReportCsvRow m (ReportRowLen a)]

-- | Fold on the elements, keys discarded.
instance ToReportRow v => ToReportRow (HashMap k v) where
    type ReportRowLen (HashMap k v) = ReportRowLen v
    type ReportRowC (HashMap k v) m = ReportRowC v m

    reportHeader _ = reportHeader (Proxy :: Proxy v)
    reportRow = foldMap reportRow
    reportCsvRow = foldMap reportCsvRow

-- | Fold on the elements, keys discarded.
instance ToReportRow v => ToReportRow (Map k v) where
    type ReportRowLen (Map k v) = ReportRowLen v
    type ReportRowC (Map k v) m = ReportRowC v m

    reportHeader _ = reportHeader (Proxy :: Proxy v)
    reportRow = foldMap reportRow
    reportCsvRow = foldMap reportCsvRow

-- | Fold on the contains.
instance ToReportRow a => ToReportRow (Vector a) where
    type ReportRowLen (Vector a) = ReportRowLen a
    type ReportRowC (Vector a) m = ReportRowC a m

    reportHeader _ = reportHeader (Proxy :: Proxy a)
    reportRow = foldMap reportRow
    reportCsvRow = foldMap reportCsvRow

instance (ToReportRow a, ToReportRow b) => ToReportRow (These a b) where
    type ReportRowLen (These a b) = PAdd (ReportRowLen a) (ReportRowLen b)

    type ReportRowC (These a b) m =
        ( SPeanoI (ReportRowLen b)
        , SPeanoI (ReportRowLen a)
        , ReportRowC a m
        , ReportRowC b m
        )

    reportHeader _ =
        let ReportHeader a = reportHeader (Proxy :: Proxy a)
            ReportHeader b = reportHeader (Proxy :: Proxy b)
        in ReportHeader (IList.append a b)

    reportRow (This a) = do
        ReportRow cls row <- reportRow a
        pure $ ReportRow cls $ IList.append row fills
      where
        fills = IList.replicateP (Proxy :: Proxy (ReportRowLen b)) $ span_ "-"
    reportRow (That b) = do
        ReportRow cls row <- reportRow b
        pure $ ReportRow cls $ IList.append fills row
      where
        fills = IList.replicateP (Proxy :: Proxy (ReportRowLen a)) $ span_ "-"
    reportRow (These a b) = do
        ReportRow cls  row  <- reportRow a
        ReportRow cls' row' <- reportRow b
        pure $ ReportRow (cls <> cls') (IList.append row row')

    reportCsvRow (This a) = do
        ReportCsvRow row <- reportCsvRow a
        pure $ ReportCsvRow $ IList.append row fills
      where
        fills = IList.replicateP (Proxy :: Proxy (ReportRowLen b)) $ pure "-"
    reportCsvRow (That b) = do
        ReportCsvRow row <- reportCsvRow b
        pure $ ReportCsvRow $ IList.append fills row
      where
        fills = IList.replicateP (Proxy :: Proxy (ReportRowLen a)) $ pure "-"
    reportCsvRow (These a b) = do
        ReportCsvRow row  <- reportCsvRow a
        ReportCsvRow row' <- reportCsvRow b
        pure $ ReportCsvRow $ IList.append row row'


-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

-- | The report.
--
-- Report is parameterised over
--
-- * @name@ - the name
-- * @params@ - the report parameteters, e.g. 'ReportGenerated'
-- * @fs@ and @a@ - report actual data
data Report (name :: Symbol) params a = Report
    { reportParams :: !params
    , reportData   :: !a
    }
    deriving (Typeable, Functor, Foldable, Traversable)

-- TODO: Eq, Show, Ord instances

data E a c where
    MkE :: ((forall m. (Monad m, Applicative m, ReportRowC a m) => m c) -> c) -> E a c

-- | Class to provide context for cell generation.
class IsReport params a where
    reportExec
        :: params
        -> E a c

defaultReportExec
    :: (ReportRowC a Identity)
    => params
    -> E a c
defaultReportExec _ = MkE (\x -> runIdentity x) -- eta expansion required for GHC7.8

readerReportExec
    :: (ReportRowC a ((->) params))
    => params
    -> E a c
readerReportExec params = MkE (\x -> x params)

-------------------------------------------------------------------------------
-- Aeson instances
-------------------------------------------------------------------------------

instance (KnownSymbol name, ToJSON params, ToJSON a)
    => ToJSON (Report name params a) where
    toJSON (Report params d) = object
        [ "name" .= symbolVal (Proxy :: Proxy name)
        , "params" .= params
        , "data" .= d
        ]

instance (KnownSymbol name, FromJSON params, FromJSON a)
    => FromJSON (Report name params a) where
    parseJSON = withObject "report" $ \obj -> Report
        <$> obj .: "params"
        <*> obj .: "data"

-- | TODO
instance ToSchema (Report name params a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

-------------------------------------------------------------------------------
-- Lucid instance
-------------------------------------------------------------------------------

instance (KnownSymbol name, ToHtml params, ToReportRow a, IsReport params a)
    => ToHtml (Report name params a) where
    toHtmlRaw _ = pure ()
    toHtml :: forall m. Monad m => Report name params a -> HtmlT m ()
    toHtml (Report params d) = case reportExec params :: E a (HtmlT m ()) of
        MkE f -> f (HtmlT . return <$> runHtmlT p)
      where
        p :: forall n. (Applicative n, Monad n, ReportRowC a n) => HtmlT n ()
        p =
          page_ (fromString title) pageParams $ do
          row_ $ large_ 12 $ h1_ $ fromString title
          row_ $ large_ 12 $ div_ [class_ "callout"] $ toHtml params
          row_ $ large_ 12 $ table_ [class_ "futu-report hover"] $ do
              thead_ $
                  tr_ $ for_ (getReportHeader $ reportHeader proxyA) $ \h ->
                      th_ $ toHtml h
              tbody_ $ for_ (reportRow d) $ \(ReportRow cls row) ->
                tr_ (cls' cls) $ traverse_ td_ row

        proxyA = Proxy :: Proxy a
        title = symbolVal (Proxy :: Proxy name)
        pageParams = defPageParams
            & pageJs .~
                [ $(embedStringFile "menrva.standalone.js")
                , $(embedStringFile "reports.js")
                ]

        cls' :: Set Text -> [Attribute]
        cls' cs
            | Set.null cs = []
            | otherwise   = [class_ $ T.intercalate " " $ toList cs]

-------------------------------------------------------------------------------
-- Cassava + Servant
-------------------------------------------------------------------------------

reportToCsvRecordList
    :: forall name params a. (ToReportRow a, IsReport params a)
    => Report name params a
    -> [[Csv.Field]]
reportToCsvRecordList (Report params a) =
    case reportExec params :: E a [[Csv.Field]] of
        MkE f -> f $ traverse g (reportCsvRow a)
  where
    g :: Monad m => ReportCsvRow m n -> m [Csv.Field]
    g = runIdentityT . sequence . toList . getReportCsv

instance (ToReportRow a, IsReport params a, EncodeOpts opt)
    => MimeRender (CSV', opt) (Report name params a)
  where
    mimeRender _ = Csv.encodeWith (encodeOpts p) . reportToCsvRecordList
      where p = Proxy :: Proxy opt
