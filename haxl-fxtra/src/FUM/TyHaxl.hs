{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module FUM.TyHaxl (
    fetchUsers,
    fetchList,
    Haxl.initDataSource,
    Haxl.FumRequest(..),
    ) where

import           Data.Vector (Vector)
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
