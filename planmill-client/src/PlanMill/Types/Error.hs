{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Error (
    PlanMillError(..),
    PlanMillErrorResponse(..),
    ) where

import PlanMill.Internal.Prelude
import Prelude                   ()

-- | TODO: it should be an 'Int'
type Code = String

data PlanMillError
    = DecodeError String String                   -- ^ Invalid response
    | ErrorResponse String PlanMillErrorResponse  -- ^ Error response
    deriving (Show, Typeable)

instance Exception PlanMillError

data PlanMillErrorResponse = PlanMillErrorResponse Code String
    deriving (Show, Typeable)

instance FromJSON PlanMillErrorResponse where
    parseJSON = withObject "Error" $ \obj ->
        PlanMillErrorResponse
            <$> obj .: "code"
            <*> obj .: "message"
