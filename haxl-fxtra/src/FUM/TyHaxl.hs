{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module FUM.TyHaxl (
    request,
    fetchUsers,
    fetchList,
    Haxl.initDataSource,
    Haxl.FumRequest(..),
    ) where

import Futurice.Prelude

import Data.Aeson (FromJSON)

import qualified FUM
import qualified FUM.Haxl    as Haxl
import           Haxl.Typed

fetchUsers
    :: IsElem' Haxl.FumRequest r (Index Haxl.FumRequest r)
    => GenTyHaxl r u (Vector FUM.User)
fetchUsers = GenTyHaxl Haxl.fetchUsers

fetchList
    :: IsElem' Haxl.FumRequest r (Index Haxl.FumRequest r)
    => FUM.ListName
    -> GenTyHaxl r u (Vector FUM.User)
fetchList = GenTyHaxl . Haxl.fetchList

request
    :: ( Show a, Typeable a, FromJSON a
       , IsElem' Haxl.FumRequest r (Index Haxl.FumRequest r)
       )
    => FUM.FUM a -> GenTyHaxl r u a
request = GenTyHaxl . Haxl.request
