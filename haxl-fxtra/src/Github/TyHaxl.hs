{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Github.TyHaxl (
    membersOf,
    userInfoFor,
    Haxl.initDataSource,
    Haxl.GithubRequest(..),
    Haxl.GithubDataSourceException,
    ) where

import Data.Vector (Vector)
import Haxl.Typed

import qualified GitHub as GH
import qualified Github.Haxl as Haxl

membersOf
    :: IsElem' Haxl.GithubRequest r (Index Haxl.GithubRequest r)
    => GH.Name GH.Organization
    -> GenTyHaxl r u (Vector GH.SimpleUser)
membersOf = GenTyHaxl . Haxl.membersOf

userInfoFor
    :: IsElem' Haxl.GithubRequest r (Index Haxl.GithubRequest r)
    => GH.Name GH.User
    -> GenTyHaxl r u GH.User
userInfoFor = GenTyHaxl . Haxl.userInfoFor
