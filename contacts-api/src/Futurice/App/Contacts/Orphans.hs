{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Contacts.Orphans () where

import Futurice.Prelude

import qualified GitHub as GH

import Codec.Picture         (Image, PixelRGBA8)
import Data.Functor.Identity (Identity)
import Lucid                 (Html, HtmlT)
import Servant.Docs          (ToSample (..))

#if !MIN_VERSION_hashable(1,2,4)
import Data.Unique (Unique, hashUnique)
#endif

import Data.Binary.Orphans ()
import Data.Binary.Tagged

instance HasStructuralInfo GH.OwnerType
instance HasStructuralInfo GH.User
instance HasStructuralInfo (GH.Name a)
instance HasStructuralInfo (GH.Id a)

instance HasSemanticVersion GH.OwnerType
instance HasSemanticVersion GH.User
instance HasSemanticVersion (GH.Name a)
instance HasSemanticVersion (GH.Id a)

instance ToSample (Html ()) where
    toSamples _ = []

-- Orphans

deriving instance Typeable PixelRGBA8
deriving instance Typeable Image
deriving instance Typeable HtmlT

#if !MIN_VERSION_transformers_compat(0,5,0)
deriving instance Typeable Identity
#endif

#if !MIN_VERSION_hashable(1,2,4)
instance Hashable Unique where
    hashWithSalt salt = hashWithSalt salt . hashUnique
#endif
