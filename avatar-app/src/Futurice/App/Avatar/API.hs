{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Avatar.API where

import Futurice.Prelude
import Prelude ()

import Codec.Picture       (DynamicImage)
import Servant
import Servant.JuicyPixels (PNG)

type AvatarAPI =
    Get '[PlainText] Text
    :<|> "avatar" :> QueryParam "url" Text
                  :> QueryParam "size" Int
                  :> QueryFlag "grey"
                  :> Get '[PNG] (Headers '[Header "Cache-Control" Text] DynamicImage)

avatarApi :: Proxy AvatarAPI
avatarApi = Proxy
