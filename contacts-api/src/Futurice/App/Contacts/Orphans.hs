{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futurice.App.Contacts.Orphans () where

import Lucid                 (Html)
import Servant.Docs          (ToSample (..))

instance ToSample (Html ()) where
    toSamples _ = []
