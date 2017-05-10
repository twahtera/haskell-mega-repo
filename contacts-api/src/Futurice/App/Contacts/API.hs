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
import Servant
import Servant.CSV.Cassava

type ContactsAPI =
    Get '[JSON] [Contact Text]
    :<|> "contacts.json" :> Get '[JSON] [Contact Text]
    :<|> "contacts.csv" :> Get '[CSV] [Contact Text]

contactsApi :: Proxy ContactsAPI
contactsApi = Proxy
