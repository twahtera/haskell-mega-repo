{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Avatar.Orphans () where

import Codec.Picture (DynamicImage)
import Data.Text     (Text)
import Servant.Docs  (ToSample (..))

instance ToSample DynamicImage where
    toSamples _ = []

instance ToSample Text where
    toSamples _ = []
