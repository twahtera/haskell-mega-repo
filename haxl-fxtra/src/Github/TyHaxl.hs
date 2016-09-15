{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Github.TyHaxl (
    request,
    membersOf,
    userInfoFor,
    Haxl.initDataSource,
    Haxl.GithubRequest(..),
    Haxl.GithubDataSourceException,
    ) where

import Futurice.Prelude
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

request
    :: (Eq a, Show a, Typeable a, IsElem' Haxl.GithubRequest r (Index Haxl.GithubRequest r))
    =>  GH.Request 'GH.RA a -> GenTyHaxl r u a
request = GenTyHaxl . Haxl.request
