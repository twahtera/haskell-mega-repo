{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.Reports.Chart (Chart (..)) where

import Prelude ()
import Futurice.Prelude
import Data.Swagger      (NamedSchema (..))
import Futurice.Generics
import GHC.TypeLits      (KnownSymbol, Symbol, symbolVal)

import qualified Graphics.Rendering.Chart.Easy as C

newtype Chart (name :: Symbol) = Chart (C.Renderable ())

instance C.ToRenderable (Chart name) where
    toRenderable (Chart r) = r

instance KnownSymbol name => ToSchema (Chart name) where
    declareNamedSchema _ = pure $ NamedSchema (Just $ "Chart" <> n) mempty
      where
        n = symbolVal (Proxy :: Proxy name) ^. packed
