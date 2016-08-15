{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.Checklist.Types.Page where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Morph (hoist)
import Futurice.Servant
import GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)
import Lucid               (Html, HtmlT, ToHtml (..))

import Data.Functor.Identity (runIdentity)

newtype Page (k :: Symbol) = Page (Html ())

instance KnownSymbol s => ToSchema (Page s) where
    declareNamedSchema _ = pure $ NamedSchema (Just name) mempty
      where
        name = symbolVal (Proxy :: Proxy s) ^. packed

instance ToHtml (Page a) where
    toHtmlRaw = toHtml
    toHtml (Page h) = hoist (return . runIdentity) h

hoistHtmlT :: (forall b. m b -> n b) -> HtmlT m a -> HtmlT n a
hoistHtmlT = undefined
