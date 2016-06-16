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
    ToReportRow1(..),
    -- * Useful helpers
    Per(..),
    ReportGenerated(..),
    -- * Aeson helpers, in aeson-1
    ToJSON1(..),
    FromJSON1(..),
    -- * Utilities
    ComposeFold,
    ) where

import Futurice.Prelude
import Futurice.Peano

import Control.Monad.Reader (runReader, Reader)
import Data.Functor.Identity (Identity (..))
import Data.Aeson.Compat (FromJSON (..), ToJSON (..), Value, object, withObject,
                          (.:), (.=))
import Data.These        (These (..))
import Data.Aeson.Types  (Parser)
import Data.Constraint   (Constraint, Dict(..))
import Data.Swagger      (ToSchema (..))
import Generics.SOP      (All, SList (..), SListI (..))
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

instance ToJSON a => ToJSON1 (Per a) where
    liftToJSON f (Per a b) = toJSON (a, f b)

instance FromJSON a => FromJSON1 (Per a) where
    liftParseJSON f j = parseJSON j >>= \(k, v) ->
        Per k <$> f v

instance ToReportRow a => ToReportRow1 (Per a) where
    type ReportRowLen1 (Per a) n = PAdd (ReportRowLen a) n

    type ReportRowC1 (Per a) m = ReportRowC a m

    liftReportHeader _ f _ =
        let ReportHeader a = reportHeader (Proxy :: Proxy a)
            ReportHeader b = f Proxy
        in ReportHeader (IList.append a b)

    liftReportRow _ f (Per a b) = do
        ReportRow cls  row  <- reportRow a
        ReportRow cls' row' <- f b
        pure $ ReportRow (cls <> cls') (IList.append row row')

-------------------------------------------------------------------------------
-- ToJSON1 / FromJSON1
-------------------------------------------------------------------------------

-- | TODO: use `ToJSON1` from @aeson-1@
class ToJSON1 f where
    liftToJSON :: (a -> Value) -> f a -> Value
    default liftToJSON :: Foldable f => (a -> Value) -> f a -> Value
    liftToJSON f = toJSON . map f . toList

class FromJSON1 f where
    liftParseJSON :: (Value -> Parser a) -> Value -> Parser (f a)

instance ToJSON1 Vector
instance FromJSON1 Vector where
    liftParseJSON p v = parseJSON v >>= traverse p

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

-- | Unary version of 'ToReportRow'.
class ToReportRow1 f where
    -- | How much fields are added.
    type ReportRowLen1 f (n :: Peano) :: Peano

    -- | Additional constrains on @HtmlT m@ monad
    type ReportRowC1 f (m :: * -> *) :: Constraint
    type ReportRowC1 f m = ()

    liftReportHeader
        :: Proxy n
        -> (Proxy a -> ReportHeader n)
        -> Proxy (f a)
        -> ReportHeader (ReportRowLen1 f n)

    liftReportRow
        :: (Monad m, ReportRowC1 f m)
        => Proxy n
        -> (a -> [ReportRow m n])
        -> f a -> [ReportRow m (ReportRowLen1 f n)]

-- | Fold on the elements, keys discarded.
instance ToReportRow1 (HashMap k) where
    type ReportRowLen1 (HashMap k) n = n

    liftReportHeader
        :: forall n a.
           Proxy n
        -> (Proxy a -> ReportHeader n)
        -> Proxy (HashMap k a)
        -> ReportHeader (ReportRowLen1 (HashMap k) n)
    liftReportHeader _ f _ = f Proxy

    liftReportRow _ = foldMap
--
-- | Fold on the elements, keys discarded.
instance ToReportRow1 (Map k) where
    type ReportRowLen1 (Map k) n = n

    liftReportHeader
        :: forall n a.
           Proxy n
        -> (Proxy a -> ReportHeader n)
        -> Proxy (Map k a)
        -> ReportHeader (ReportRowLen1 (Map k) n)
    liftReportHeader _ f _ = f Proxy

    liftReportRow _ = foldMap

-- | Fold on the contains.
instance ToReportRow1 Vector where
    type ReportRowLen1 Vector n = n

    liftReportHeader
        :: forall n a.
           Proxy n
        -> (Proxy a -> ReportHeader n)
        -> Proxy (Vector a)
        -> ReportHeader (ReportRowLen1 Vector n)
    liftReportHeader _ f _ = f Proxy

    liftReportRow _ = foldMap

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

-- | Nested application
--
-- @
-- >>> :kind! ComposeFold '[[], Maybe, Either Bool] Int
-- ComposeFold '[[], Maybe, Either Bool] Int :: *
-- = [Maybe (Either Bool Int)]
-- @
type family ComposeFold (fs :: [* -> *]) (a :: *) :: * where
    ComposeFold '[]       a = a
    ComposeFold (f ': fs) a = f (ComposeFold fs a)

-- | The report.
--
-- Report is parameterised over
--
-- * @name@ - the name
-- * @params@ - the report parameteters, e.g. 'ReportGenerated'
-- * @fs@ and @a@ - report actual data
data Report (name :: Symbol) params fs a = Report
    { reportParams :: !params
    , reportData   :: !(ComposeFold fs a)
    }
    deriving (Typeable)

-- TODO: Eq, Show, Ord instances

-- | Class to provide context for cell generation.
class IsReport params fs a where
    reportExec
        :: Proxy fs -> Proxy a -> params
        -> (forall m. Dict (Monad m, FoldReportRowC fs a m) -> HtmlT m ())
        -> Html ()

defaultReportExec
    :: (FoldReportRowC fs a Identity)
    => Proxy fs -> Proxy a -> params
    -> (forall m. Dict (Monad m, FoldReportRowC fs a m) -> HtmlT m ())
    -> Html ()
defaultReportExec _ _ _ f = f Dict

ntHtmlT :: (forall b. m b -> n b) -> HtmlT m a -> HtmlT n a
ntHtmlT nt (HtmlT x) = HtmlT (nt x)

readerReportExec
    :: (FoldReportRowC fs a (Reader params))
    => Proxy fs -> Proxy a -> params
    -> (forall m. Dict (Monad m, FoldReportRowC fs a m) -> HtmlT m ())
    -> Html ()
readerReportExec _ _ params f = ntHtmlT (Identity . flip runReader params) (f Dict)

-------------------------------------------------------------------------------
-- Aeson instances
-------------------------------------------------------------------------------

instance (KnownSymbol name, ToJSON params, All ToJSON1 fs, ToJSON a)
    => ToJSON (Report name params fs a) where
    toJSON (Report params fs) = object
        [ "name" .= symbolVal (Proxy :: Proxy name)
        , "params" .= params
        , "data" .= foldToJSON (Proxy :: Proxy fs) (Proxy :: Proxy a) fs
        ]

foldToJSON
    :: forall fs a. (SListI fs, All ToJSON1 fs, ToJSON a)
    => Proxy fs -> Proxy a -> ComposeFold fs a -> Value
foldToJSON _ pa x = case sList :: SList fs of
    SNil  -> toJSON x
    SCons -> consCase
  where
    consCase :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs')) => Value
    consCase = liftToJSON (foldToJSON (Proxy :: Proxy fs') pa) x

instance (KnownSymbol name, FromJSON params, All FromJSON1 fs, FromJSON a)
    => FromJSON (Report name params fs a) where
    parseJSON = withObject "report" $ \obj -> Report
        <$> obj .: "params"
        <*> (obj .: "data" >>= foldParseJSON (Proxy :: Proxy fs) (Proxy :: Proxy a))

foldParseJSON
    :: forall fs a. (SListI fs, All FromJSON1 fs, FromJSON a)
    => Proxy fs -> Proxy a -> Value -> Parser (ComposeFold fs a)
foldParseJSON _ pa v = case sList :: SList fs of
    SNil  -> parseJSON v
    SCons -> consCase
  where
    consCase
        :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs'))
        => Parser (ComposeFold fs a)
    consCase = liftParseJSON (foldParseJSON (Proxy :: Proxy fs') pa) v

-- | TODO
instance ToSchema (Report name params fs a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

-------------------------------------------------------------------------------
-- Lucid instance
-------------------------------------------------------------------------------

instance (KnownSymbol name, ToHtml params, All ToReportRow1 fs, ToReportRow a, IsReport params fs a)
    => ToHtml (Report name params fs a) where
    toHtmlRaw _ = pure ()
    toHtml (Report params fs) = HtmlT . return . runIdentity . runHtmlT $
        reportExec proxyFs proxyA params p
      where
        p :: Dict (Monad m, FoldReportRowC fs a m) -> HtmlT m ()
        p Dict =
          page_ (fromString title) $ do
          row_ $ large_ 12 $ h1_ $ fromString title
          row_ $ large_ 12 $ div_ [class_ "callout"] $ toHtml params
          row_ $ large_ 12 $ table_ [class_ "hover"] $ do
              thead_ $
                  tr_ $ for_ (getReportHeader $ foldReportHeader proxyFs proxyA Proxy) $ \h ->
                      th_ $ toHtml h
              tbody_ $ for_ (foldReportRow proxyFs proxyA fs) $ \(ReportRow cls row) ->
                tr_ (cls' cls) $ traverse_ td_ row

        proxyA = Proxy :: Proxy a
        proxyFs = Proxy :: Proxy fs
        title = symbolVal (Proxy :: Proxy name)

        cls' :: Set Text -> [Attribute]
        cls' cs
            | Set.null cs = []
            | otherwise   = [class_ $ T.intercalate " " $ toList cs]

type family FoldReportLen fs a where
    FoldReportLen '[]       a = ReportRowLen a
    FoldReportLen (f ': fs) a = ReportRowLen1 f (FoldReportLen fs a)

foldReportHeader
    :: forall fs a. (SListI fs, All ToReportRow1 fs, ToReportRow a)
    => Proxy fs -> Proxy a -> Proxy (ComposeFold fs a)
    -> ReportHeader (FoldReportLen fs a)
foldReportHeader _ pa _ = case sList :: SList fs of
    SNil  -> reportHeader pa
    SCons -> consCase
      where
        consCase
            :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs'))
            => ReportHeader (ReportRowLen1 f (FoldReportLen fs' a))
        consCase = liftReportHeader
            Proxy
            (foldReportHeader (Proxy :: Proxy fs') pa)
            (Proxy :: Proxy (f (ComposeFold fs' a)))

type family FoldReportRowC fs a m :: Constraint where
    FoldReportRowC '[]       a m = ReportRowC a m
    FoldReportRowC (f ': fs) a m = (ReportRowC1 f m, FoldReportRowC fs a m)

foldReportRow
    :: forall fs a m.
       ( SListI fs
       , All ToReportRow1 fs, ToReportRow a
       , FoldReportRowC fs a m, Monad m
       )
    => Proxy fs -> Proxy a -> ComposeFold fs a
    -> [ReportRow m (FoldReportLen fs a)]
foldReportRow _ pa v = case sList :: SList fs of
    SNil  -> reportRow v
    SCons -> consCase
  where
    consCase
        :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs'))
        => [ReportRow m (ReportRowLen1 f (FoldReportLen fs' a))]
    consCase = liftReportRow Proxy (foldReportRow (Proxy :: Proxy fs') pa) v
