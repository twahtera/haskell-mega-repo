{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Enumerations (
    enumerationForField,
    enumerationValue,
    ) where

import PlanMill.Internal.Prelude

import Data.Constraint (Dict (..))
import Data.Reflection (reifySymbol)
import GHC.TypeLits    (KnownSymbol, symbolVal)

import qualified Data.IntMap.Strict as IM
import qualified Data.Text          as T

import Control.Monad.PlanMill
import PlanMill.Endpoints
import PlanMill.Types

class HasMeta entity where
    metaPath :: Proxy entity -> UrlParts

instance HasMeta User where
    metaPath _ = t "users" // t "meta"
      where t = id :: Text -> Text

meta :: HasMeta entity => Proxy entity -> PlanMill Meta
meta = planMillGet . metaPath

enumerationForField
    :: forall entity field m.
        ( HasMeta entity
        , KnownSymbol field
        , MonadPlanMill m
        , MonadPlanMillC m Meta
        , ForallFSymbol (MonadPlanMillC m) EnumDesc
        )
    => Proxy entity -> Proxy field
    -> m (Maybe SomeEnumDesc)
enumerationForField entityProxy fieldNameProxy = do
    m <- planmillAction (meta entityProxy)
    case lookupFieldEnum m (T.pack $ symbolVal fieldNameProxy) of
        Nothing -> return Nothing -- TODO: Throw an unknown field exception?
        Just enumName  -> reifyTextSymbol enumName $ \enumProxy ->
            case instFSymbol (Proxy :: Proxy (MonadPlanMillC m)) (Proxy :: Proxy EnumDesc) enumProxy of
                Dict -> do
                    desc <- planmillAction $ enumerations enumProxy
                    return $ Just $ MkSomeEnumDesc desc

enumerationValue
    :: forall entity field m.
        ( HasMeta entity
        , KnownSymbol field
        , MonadPlanMill m
        , MonadPlanMillC m Meta
        , ForallFSymbol (MonadPlanMillC m) EnumDesc
        )
    => EnumValue entity field
    -> Text  -- ^ Default text
    -> m Text
enumerationValue (EnumValue value) defaultText = do
    mDesc <- enumerationForField (Proxy :: Proxy entity) (Proxy :: Proxy field)
    case mDesc of
        Nothing   -> return defaultText
        Just (MkSomeEnumDesc (EnumDesc im)) -> case IM.lookup value im of
            Nothing        -> return defaultText
            Just textValue -> return textValue

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

reifyTextSymbol :: forall r. Text -> (forall n. KnownSymbol n => Proxy n -> r) -> r
reifyTextSymbol t = reifySymbol (T.unpack t)
