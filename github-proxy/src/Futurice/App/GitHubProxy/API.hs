{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.GitHubProxy.API where

import Futurice.Prelude
import Prelude ()

import Futurice.GitHub  (SomeRequest, SomeResponse)
import Servant
import Servant.Binary.Tagged (BINARYTAGGED)

type GitHubProxyAPI =
    Get '[JSON] Text
    :<|> "github-haxl" :> ReqBody '[JSON] [SomeRequest] :> Post '[BINARYTAGGED] [Either Text SomeResponse]

githubProxyApi :: Proxy GitHubProxyAPI 
githubProxyApi = Proxy
