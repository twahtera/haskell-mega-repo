{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.BinaryFromJSON (
    BinaryFromJSON,
    ) where

import Futurice.Prelude hiding (lookup)

import Data.Aeson                       (FromJSON)
import Data.Binary.Tagged               (HasSemanticVersion, HasStructuralInfo)
import Futurice.Constraint.ForallSymbol (Dict (..), ForallFSymbol (..))
import GHC.TypeLits                     (KnownSymbol)

class (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a, Show a, Typeable a) => BinaryFromJSON a
instance (Binary a, HasSemanticVersion a, HasStructuralInfo a, FromJSON a, Show a, Typeable a) => BinaryFromJSON a
instance ( ForallFSymbol FromJSON e
         , ForallFSymbol Binary e
         , ForallFSymbol HasStructuralInfo e
         , ForallFSymbol HasSemanticVersion e
         , ForallFSymbol Show e
         , ForallFSymbol Typeable e
         )
    => ForallFSymbol BinaryFromJSON e
  where
    instFSymbol :: forall k. KnownSymbol k => Dict (BinaryFromJSON (e k))
    instFSymbol =
        case proxies of
            (Dict, Dict, Dict, Dict, Dict, Dict) -> Dict
      where
        proxies =
            ( instFSymbol :: Dict (FromJSON (e k))
            , instFSymbol :: Dict (Binary (e k))
            , instFSymbol :: Dict (HasStructuralInfo (e k))
            , instFSymbol :: Dict (HasSemanticVersion (e k))
            , instFSymbol :: Dict (Show (e k))
            , instFSymbol :: Dict (Typeable (e k))
            )
