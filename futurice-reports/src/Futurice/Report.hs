{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | The report i.e. fancy table.
module Futurice.Report (
    -- * Reports
    Report(..),
    ReportHeader(..),
    ReportRow(..),
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

import Futurice.Prelude
import Futurice.Peano

import Control.Monad.Reader (runReader, Reader)
import Data.Functor.Identity (Identity (..))
import Data.Aeson.Compat (FromJSON (..), ToJSON (..), object, withObject,
                          (.:), (.=))
import Data.These        (These (..))
import Data.Constraint   (Constraint, Dict(..))
import Data.Swagger      (ToSchema (..))
import GHC.TypeLits      (KnownSymbol, Symbol, symbolVal)
import Lucid hiding (for_)
import Lucid.Base (HtmlT(..))
import Lucid.Foundation.Futurice (large_, page_, row_)

import qualified Futurice.IC as IList
import qualified Data.Text as T
import qualified Data.Set as Set

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

-------------------------------------------------------------------------------
-- Lucid
-------------------------------------------------------------------------------

-- | Header of the report.
newtype ReportHeader (n :: Peano) = ReportHeader { getReportHeader :: IList.IList n Text }

-- | Single row of the report.
data ReportRow m (n :: Peano) = ReportRow
    { getReportRowCls :: Set Text
    , getReportRow :: !(IList.IList n (HtmlT m ()))
    }

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
        :: (Monad m, ReportRowC a m)
        => a -> [ReportRow m (ReportRowLen a)]

-- | Fold on the elements, keys discarded.
instance ToReportRow v => ToReportRow (HashMap k v) where
    type ReportRowLen (HashMap k v) = ReportRowLen v
    type ReportRowC (HashMap k v) m = ReportRowC v m

    reportHeader _ = reportHeader (Proxy :: Proxy v)
    reportRow = foldMap reportRow

-- | Fold on the elements, keys discarded.
instance ToReportRow v => ToReportRow (Map k v) where
    type ReportRowLen (Map k v) = ReportRowLen v
    type ReportRowC (Map k v) m = ReportRowC v m

    reportHeader _ = reportHeader (Proxy :: Proxy v)
    reportRow = foldMap reportRow

-- | Fold on the contains.
instance ToReportRow a => ToReportRow (Vector a) where
    type ReportRowLen (Vector a) = ReportRowLen a
    type ReportRowC (Vector a) m = ReportRowC a m

    reportHeader _ = reportHeader (Proxy :: Proxy a)
    reportRow = foldMap reportRow

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

-------------------------------------------------------------------------------
-- Cassava
-------------------------------------------------------------------------------

-- TODO: add cassava support for 'Report' if needed

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

-- | Class to provide context for cell generation.
class IsReport params a where
    reportExec
        :: Proxy a -> params
        -> (forall m. Dict (Monad m, ReportRowC a m) -> HtmlT m ())
        -> Html ()

defaultReportExec
    :: (ReportRowC a Identity)
    => Proxy a -> params
    -> (forall m. Dict (Monad m, ReportRowC a m) -> HtmlT m ())
    -> Html ()
defaultReportExec _ _ f = f Dict

ntHtmlT :: (forall b. m b -> n b) -> HtmlT m a -> HtmlT n a
ntHtmlT nt (HtmlT x) = HtmlT (nt x)

readerReportExec
    :: (ReportRowC a (Reader params))
    => Proxy a -> params
    -> (forall m. Dict (Monad m, ReportRowC a m) -> HtmlT m ())
    -> Html ()
readerReportExec _ params f = ntHtmlT (Identity . flip runReader params) (f Dict)

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
    toHtml (Report params d) = HtmlT . return . runIdentity . runHtmlT $
        reportExec proxyA params p
      where
        p :: Dict (Monad m, ReportRowC a m) -> HtmlT m ()
        p Dict =
          page_ (fromString title) $ do
          row_ $ large_ 12 $ h1_ $ fromString title
          row_ $ large_ 12 $ div_ [class_ "callout"] $ toHtml params
          row_ $ large_ 12 $ table_ [class_ "hover"] $ do
              thead_ $
                  tr_ $ for_ (getReportHeader $ reportHeader proxyA) $ \h ->
                      th_ $ toHtml h
              tbody_ $ for_ (reportRow d) $ \(ReportRow cls row) ->
                tr_ (cls' cls) $ traverse_ td_ row

        proxyA = Proxy :: Proxy a
        title = symbolVal (Proxy :: Proxy name)

        cls' :: Set Text -> [Attribute]
        cls' cs
            | Set.null cs = []
            | otherwise   = [class_ $ T.intercalate " " $ toList cs]
