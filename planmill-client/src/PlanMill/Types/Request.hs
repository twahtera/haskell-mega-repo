{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Request (
    -- * Request type
    PlanMill (..),
    -- * Smart constructors
    planMillGet,
    planMillGetQs,
    planMillPagedGet,
    planMillPagedGetQs,
    planMillPost,
    planMillPostNoResponse,
    -- * Helpers
    QueryString,
    requestUrlParts,
    ) where

import PlanMill.Internal.Prelude

import Data.Aeson.Compat      (encode)
import PlanMill.Types.UrlPart (ToUrlParts (..), UrlParts)

import qualified Data.ByteString.Lazy as LBS

type QueryString = Map Text Text

-- | Planmill API request.
--
-- We can have different constraints on result type,
-- for example to be able to cache responses.
--
-- First argument in 'PlanMillPost' is for empty outputs
data PlanMill a where
    PlanMillGet         :: QueryString -> UrlParts -> PlanMill a
    -- TODO: add "max iteration" or "max size" limit
    PlanMillPagedGet    :: QueryString -> UrlParts -> PlanMill (Vector a)
    PlanMillPost        :: Maybe (a :~: ()) -> LBS.ByteString -> UrlParts -> PlanMill a

deriving instance Eq (PlanMill a)
deriving instance Ord (PlanMill a)

instance Show (PlanMill a) where
    showsPrec d r = case r of
        PlanMillGet qs ps -> showParen (d > appPrec)
            $ showString "PlanMillGet "
            . showsPrec (appPrec + 1) qs
            . showString " "
            . showsPrec (appPrec + 1) ps
        PlanMillPagedGet qs ps -> showParen (d > appPrec)
            $ showString "PlanMillPagedGet "
            . showsPrec (appPrec + 1) qs
            . showString " "
            . showsPrec (appPrec + 1) ps
        PlanMillPost em body ps -> showParen (d > appPrec)
            $ showString "PlanMillPost "
            . showsPrec (appPrec + 1) em
            . showString " "
            . showsPrec (appPrec + 1) body
            . showString " "
            . showsPrec (appPrec + 1) ps
      where
        appPrec = 10 :: Int

instance Hashable (PlanMill a) where
    hashWithSalt salt (PlanMillGet qs ps) =
        salt `hashWithSalt` (0 :: Int)
             `hashWithSalt` qs
             `hashWithSalt` ps
    hashWithSalt salt (PlanMillPagedGet qs ps) =
        salt `hashWithSalt` (1 :: Int)
             `hashWithSalt` qs
             `hashWithSalt` ps
    hashWithSalt salt (PlanMillPost _ body ps) =
        salt `hashWithSalt` (2 :: Int)
             `hashWithSalt` body
             `hashWithSalt` ps

instance NFData (PlanMill a) where
    rnf (PlanMillGet qs ps)      = rnf qs `seq` rnf ps
    rnf (PlanMillPagedGet qs ps) = rnf qs `seq` rnf ps
    rnf (PlanMillPost e body ps) = rnf e `seq` rnf body `seq` rnf ps

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

planMillGet :: (ToUrlParts p) => p -> PlanMill a
planMillGet = PlanMillGet mempty. toUrlParts

-- | Like 'planMillGet', but with 'QueryString'.
planMillGetQs :: (ToUrlParts p) => QueryString -> p -> PlanMill a
planMillGetQs qs = PlanMillGet qs . toUrlParts

-- | Like 'planMillGet', but for paged requests (i.e. collections)
planMillPagedGet :: (ToUrlParts p) => p -> PlanMill (Vector a)
planMillPagedGet = PlanMillPagedGet mempty . toUrlParts

-- | Like 'planMillPagedGet', but with 'QueryString'.
planMillPagedGetQs :: (ToUrlParts p)
                   => QueryString -> p -> PlanMill (Vector a)
planMillPagedGetQs qs = PlanMillPagedGet qs . toUrlParts

planMillPost :: (ToJSON d, ToUrlParts p) => d -> p -> PlanMill a
planMillPost d = PlanMillPost Nothing (encode d) . toUrlParts

planMillPostNoResponse :: (ToJSON d, ToUrlParts p) => d -> p -> PlanMill ()
planMillPostNoResponse d = PlanMillPost (Just Refl) (encode d) . toUrlParts

-- | Get an path part as 'UrlParts' of the request
--
-- > req <- parseUrlThrow $ baseUrl <> fromUrlParts (requestUrlParts planmillRequest)
requestUrlParts :: PlanMill a -> UrlParts
requestUrlParts (PlanMillGet _ ps)      = ps
requestUrlParts (PlanMillPagedGet _ ps) = ps
requestUrlParts (PlanMillPost _ _ ps)   = ps
