{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Avatar.API where

import Prelude        ()
import Futurice.Prelude

import Codec.Picture   (DynamicImage)
import Futurice.Colour

import Servant
import Servant.Cache.Class    (DynMapCache)
import Servant.Docs
import Servant.Futurice.Utils
import Servant.JuicyPixels    (PNG)

import Futurice.App.Avatar.Orphans ()

type AvatarAPI =
    Get '[PlainText] Text
    :<|> "avatar" :> QueryParam "url" Text
                  :> QueryParam "size" Int
                  :> QueryFlag "grey"
                  :> Get '[PNG] (Headers '[Header "Cache-Control" Text] DynamicImage)

type AvatarAPI' = AvatarAPI :<|> AuxAPI ('FutuAccent 'AF5 'AC2)

avatarApi :: Proxy AvatarAPI
avatarApi = Proxy

avatarApi' :: Proxy AvatarAPI'
avatarApi' = Proxy

serverAvatarApi :: DynMapCache -> UTCTime -> String -> Server AvatarAPI -> Server AvatarAPI'
serverAvatarApi cache start now server = server :<|> serverAuxApi cache start now avatarApi mempty

instance ToParam (QueryParam "url" Text) where
  toParam _ =
    DocQueryParam "url"
                  ["example.org/com"]
                  "The url of the image"
                  Normal

instance ToParam (QueryParam "size" Int) where
  toParam _ =
    DocQueryParam "size"
                  ["32"]
                  "The size of resulting avatar"
                  Normal

instance ToParam (QueryFlag "grey") where
  toParam _ =
    DocQueryParam "grey"
                  ["true", "false"]
                  "The greyscale image"
                 Normal
