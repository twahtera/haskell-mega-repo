{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
module PlanMill.Types.TimeBalance
    ( TimeBalance(..)
    , tbMinutes
    )
    where

import PlanMill.Internal.Prelude
import Prelude                   ()

newtype TimeBalance = TimeBalance { _tbMinutes :: Double }
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable TimeBalance
instance NFData TimeBalance
instance AnsiPretty TimeBalance
instance Binary TimeBalance
instance HasStructuralInfo TimeBalance where structuralInfo = sopStructuralInfo
instance HasSemanticVersion TimeBalance

instance FromJSON TimeBalance where
    parseJSON = withObject "TimeBalance" $ \obj ->
        TimeBalance <$> obj .: "amount"

makeLenses ''TimeBalance
deriveGeneric ''TimeBalance
