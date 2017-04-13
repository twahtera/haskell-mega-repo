{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Checklist.Types.TaskRole where

import Prelude ()
import Futurice.Prelude
import Control.Lens      (Index, IxValue, Ixed (..))
import Data.Aeson.Compat (Value (String), withText)
import Data.Distributive (Distributive (..))
import Data.Functor.Rep  (Representable (..), distributeRep, liftR2, pureRep)
import Data.Swagger
       (SwaggerType (SwaggerString), ToParamSchema (..), enum_, type_)
import Futurice.Generics
import Servant           (FromHttpApiData (..), ToHttpApiData (..))

import qualified Data.Map  as Map
import qualified Data.Text as T

-- | States of the tasks
data TaskRole
    = TaskRoleIT
    | TaskRoleHR
    | TaskRoleSupervisor
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''TaskRole
deriveGeneric ''TaskRole

instance NFData TaskRole

instance Arbitrary TaskRole where
    arbitrary = sopArbitrary
    shrink    = sopShrink

taskRoleToText :: TaskRole -> Text
taskRoleToText TaskRoleIT         = "IT"
taskRoleToText TaskRoleHR         = "HR"
taskRoleToText TaskRoleSupervisor = "Supervisor"

taskRoleFromText :: Text -> Maybe TaskRole
taskRoleFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ taskRoleToText x, x)) [minBound .. maxBound]

_TaskRole :: Prism' Text TaskRole
_TaskRole = prism' taskRoleToText taskRoleFromText

instance ToParamSchema TaskRole where
    toParamSchema _ = mempty
        & type_ .~ SwaggerString
        & enum_ ?~ (String . taskRoleToText <$> [minBound .. maxBound])

instance ToJSON TaskRole where
    toJSON = String . taskRoleToText

instance FromJSON TaskRole where
    parseJSON = withText "TaskRole" $ \t ->
        maybe (fail $ "invalid taskRole " <> t ^. unpacked) pure $ t ^? _TaskRole

instance FromHttpApiData TaskRole where
    parseUrlPiece t =
        maybe (Left $ "invalid taskRole " <> t) Right $ t ^? _TaskRole

instance ToHttpApiData TaskRole where
    toUrlPiece = taskRoleToText

-------------------------------------------------------------------------------
-- PerTaskRole container
-------------------------------------------------------------------------------

data PerTaskRole a = PerTaskRole !a !a !a
  deriving (Functor)

instance NFData a => NFData (PerTaskRole a) where
    rnf (PerTaskRole x y z) =
        rnf x `seq` rnf y `seq` rnf z

type instance Index (PerTaskRole a) = TaskRole
type instance IxValue (PerTaskRole a) = a

instance Ixed (PerTaskRole a) where
    ix TaskRoleIT f (PerTaskRole x y z) = (\a -> PerTaskRole a y z) <$> f x
    ix TaskRoleHR f (PerTaskRole x y z) = (\a -> PerTaskRole x a z) <$> f y
    ix TaskRoleSupervisor f (PerTaskRole x y z) = (\a -> PerTaskRole x y a) <$> f z

instance Semigroup a => Semigroup (PerTaskRole a) where
    (<>) = liftR2 (<>)

instance Monoid a => Monoid (PerTaskRole a) where
    mempty = pureRep mempty
    mappend = liftR2 mappend

instance Distributive PerTaskRole where
    distribute = distributeRep

instance Representable PerTaskRole where
    type Rep PerTaskRole = TaskRole

    tabulate f = PerTaskRole
        (f TaskRoleIT)
        (f TaskRoleHR)
        (f TaskRoleSupervisor)

    index (PerTaskRole x _ _) TaskRoleIT = x
    index (PerTaskRole _ x _) TaskRoleHR = x
    index (PerTaskRole _ _ x) TaskRoleSupervisor = x
