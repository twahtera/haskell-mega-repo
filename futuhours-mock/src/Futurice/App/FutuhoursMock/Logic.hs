{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FutuhoursMock.Logic (
    projectEndpoint,
    ) where

import Futurice.App.FutuhoursMock.Types
import Futurice.Prelude
import Prelude ()

import Test.QuickCheck (arbitrary, sample')
import Data.Vector.Lens (vector)

projectEndpoint :: Ctx -> IO (Vector Project)
projectEndpoint _ctx = do
    projects <- sample' arbitrary
    pure $ projects ^. vector
