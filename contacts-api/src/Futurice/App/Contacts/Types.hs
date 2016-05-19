{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
module Futurice.App.Contacts.Types (
    Tri(..),
    ContactFD(..),
    ContactGH(..),
    Contact(..),
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Aeson.TH
import Data.Char     (toLower)
import Servant.Docs  (ToSample (..))

import Futurice.App.Contacts.Types.Tri

data ContactFD avatar = ContactFD
    { cfdId     :: !Int       -- ^ Identifier
    , cfdNick   :: !Text      -- ^ Nick
    , cfdAvatar :: !avatar    -- ^ Avatar
    }
    deriving ( Eq, Ord, Show, Read, Generic, Typeable
             , Functor, Foldable, Traversable)

instance NFData a => NFData (ContactFD a)
$(deriveJSON
    defaultOptions{fieldLabelModifier = map toLower . drop 3 }
    ''ContactFD)

data ContactGH avatar = ContactGH
    { cghNick   :: !Text
    , cghAvatar :: !avatar
    }
    deriving ( Eq, Ord, Show, Read, Generic, Typeable
             , Functor, Foldable, Traversable)

instance NFData a => NFData (ContactGH a)
$(deriveJSON
    defaultOptions{fieldLabelModifier = map toLower . drop 3 }
    ''ContactGH)

data Contact avatar = Contact
    { contactLogin    :: !Text
    , contactFirst    :: !Text
    , contactName     :: !Text
    , contactEmail    :: !Text
    , contactPhones   :: ![Text]
    , contactTitle    :: !(Maybe Text)
    , contactThumb    :: !avatar
    , contactImage    :: !Text
    , contactFlowdock :: !(Tri (ContactFD avatar))
    , contactGithub   :: !(Tri (ContactGH avatar))
    }
    deriving ( Eq, Ord, Show, Read, Generic, Typeable
             , Functor, Foldable, Traversable)

instance NFData a => NFData (Contact a)

instance ToSample (Contact Text) where
    toSamples _ = [("", contact)]
      where
        contact = Contact "fbar"
                          "Foo"
                          "Foo Bar"
                          "foo.bar@example.com"
                          ["555-HASKELL"]
                          (Just "Head of Dummies")
                          "01234567910"
                          "http://example.com/image.jpg"
                          Unknown
                          Unknown

-- TH slices

$(deriveJSON
    defaultOptions{fieldLabelModifier = map toLower . drop 7 }
    ''Contact)
