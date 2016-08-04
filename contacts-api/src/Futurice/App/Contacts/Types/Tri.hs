{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Futurice.App.Contacts.Types.Tri (Tri(..), lessSure) where

import Futurice.Prelude
import Prelude ()

import Control.Monad (ap)
import Data.Char     (toLower)

import qualified Data.Aeson.TH as A
import qualified Data.Swagger  as S

-- | This essentially is @'WriterT' 'All' 'Maybe' a@,
-- with nice 'Semigroup' / 'Monoid' instance.
data Tri a = Sure a
           | Unsure a
           | Unknown
  deriving (Eq, Ord, Show, Read, Generic, Functor, Foldable, Traversable)

instance Monad Tri where
    return = pure
    Sure a    >>= f  = f a
    Unsure a  >>= f  = case f a of
                           Sure b  -> Unsure b
                           x       -> x
    Unknown   >>= _  = Unknown

instance Applicative Tri where
    pure = Sure
    (<*>) = ap

instance Semigroup (Tri a) where
    x@Sure{} <> _       = x
    _ <> x@Sure{}       = x
    x@Unsure{} <> _     = x
    _ <> x@Unsure{}     = x
    Unknown <> Unknown  = Unknown

instance Monoid (Tri a) where
    mempty = Unknown
    mappend = (<>)

instance NFData a => NFData (Tri a)
$(A.deriveJSON A.defaultOptions
    { A.constructorTagModifier = map toLower } ''Tri)
instance S.ToSchema a => S.ToSchema (Tri a) where
    declareNamedSchema = S.genericDeclareNamedSchema S.defaultSchemaOptions
        { S.constructorTagModifier = map toLower }

lessSure :: Tri a -> Tri a
lessSure (Sure a) = Unsure a
lessSure x        = x
