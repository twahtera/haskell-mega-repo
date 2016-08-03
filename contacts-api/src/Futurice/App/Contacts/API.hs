{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Contacts.API where

import Futurice.Prelude
import Prelude ()

import Futurice.App.Contacts.Types
import Futurice.Servant
import Servant

import qualified Data.Text as T

type ContactsAPI =
    Get '[JSON] [Contact T.Text]
    :<|> "contacts.json" :> Get '[JSON] [Contact T.Text]

type ContactsAPI' = FuturiceAPI ContactsAPI ('FutuAccent 'AF2 'AC3)

contactsAPI :: Proxy ContactsAPI
contactsAPI = Proxy

contactsAPI' :: Proxy ContactsAPI'
contactsAPI' = Proxy
