{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-- | TODO: move to own package
module Futurice.Report (
    Report(..),
    ReportGenerated(..),
    -- * Aeson helpers
    ToJSON1(..),
    FromJSON1(..),
    -- * 
    ToReportRow(..),
    ToReportRow1(..),
    ) where

import Futurice.Prelude

import Data.Aeson.Compat (FromJSON (..), ToJSON (..), Value, object, withObject,
                          (.:), (.=))
import Data.Aeson.Types  (Parser)
import Data.Swagger      (ToSchema (..))
import Generics.SOP      (All, SList (..), SListI (..))
import GHC.TypeLits      (KnownSymbol, Symbol, symbolVal)
import Lucid hiding (for_)
import Lucid.Foundation.Futurice (large_, page_, row_)

-------------------------------------------------------------------------------
-- ReportGenerated
-------------------------------------------------------------------------------

newtype ReportGenerated = ReportGenerated { getReportGenerated :: UTCTime }

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
-- ToJSON1
-------------------------------------------------------------------------------

-- | TODO: use `ToJSON1` from @aeson-0.12@
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

class ToReportRow a where
    reportHeader ::Proxy a -> [Text]
    reportRow :: Monad m => a -> [[HtmlT m ()]]

class ToReportRow1 f where
    liftReportHeader
        :: (Proxy a -> [Text])
        -> Proxy (f a) -> [Text]
    liftReportRow
        :: Monad m
        => (a -> [[HtmlT m ()]])
        -> f a -> [[HtmlT m ()]]

instance ToReportRow1 (HashMap k) where
    liftReportHeader = go
      where
        go :: forall a. (Proxy a -> [Text]) -> Proxy (HashMap k a) -> [Text]
        go f _ = f Proxy

    liftReportRow = foldMap

instance ToReportRow1 Vector where
    liftReportHeader = go
      where
        go :: forall a. (Proxy a -> [Text]) -> Proxy (Vector a) -> [Text]
        go f _ = f Proxy

    liftReportRow = foldMap 

-------------------------------------------------------------------------------
-- Cassava
-------------------------------------------------------------------------------

-- TODO: add cassava support for 'Report' if needed

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type family ComposeFold (fs :: [* -> *]) (a :: *) :: * where
    ComposeFold '[]       a = a
    ComposeFold (f ': fs) a = f (ComposeFold fs a)

data Report (name :: Symbol) params fs a = Report
    { reportParams :: !params
    , reportData   :: !(ComposeFold fs a)
    }
    deriving (Typeable)

-- TODO: Eq, Show, Ord instances

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
    consCase :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs')) => Parser (ComposeFold fs a)
    consCase = liftParseJSON (foldParseJSON (Proxy :: Proxy fs') pa) v

-- | TODO
instance ToSchema (Report name params fs a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

instance (KnownSymbol name, ToHtml params, All ToReportRow1 fs, ToReportRow a)
    => ToHtml (Report name params fs a) where
    toHtmlRaw _ = pure ()
    toHtml (Report params fs) = page_ (fromString title) $ do
        row_ $ large_ 12 $ fromString title
        row_ $ large_ 12 $ toHtml params
        row_ $ large_ 12 $ table_ $ do
            tr_ $ for_ (foldReportHeader (Proxy :: Proxy fs) (Proxy :: Proxy a) Proxy) $ \h ->
                th_ $ toHtml h
            for_ (foldReportRow (Proxy :: Proxy fs) (Proxy :: Proxy a) fs) $ tr_ . traverse_ td_
      where
        title = symbolVal (Proxy :: Proxy name)

foldReportHeader
    :: forall fs a. (SListI fs, All ToReportRow1 fs, ToReportRow a)
    => Proxy fs -> Proxy a -> Proxy (ComposeFold fs a) -> [Text]
foldReportHeader _ pa _ = case sList :: SList fs of
    SNil  -> reportHeader pa
    SCons -> consCase
      where
        consCase :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs')) => [Text]
        consCase = liftReportHeader (foldReportHeader (Proxy :: Proxy fs') pa) (Proxy :: Proxy (f (ComposeFold fs' a)))

foldReportRow
    :: forall fs a m. (SListI fs, All ToReportRow1 fs, ToReportRow a, Monad m)
    => Proxy fs -> Proxy a -> ComposeFold fs a -> [[HtmlT m ()]]
foldReportRow _ pa v = case sList :: SList fs of
    SNil  -> reportRow v
    SCons -> consCase
  where
    consCase :: forall f (fs' :: [* -> *]). (fs ~ (f ': fs')) => [[HtmlT m ()]]
    consCase = liftReportRow (foldReportRow (Proxy :: Proxy fs') pa) v
    
