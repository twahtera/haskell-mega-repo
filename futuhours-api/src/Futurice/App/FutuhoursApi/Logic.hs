module Futurice.App.FutuhoursApi.Logic (
    projectEndpoint,
    userEndpoint,
    hoursEndpoint,
    entryEndpoint,
    entryIdEndpoint,
    entryDeleteEndpoint,
    ) where

import Prelude ()
import Futurice.Prelude
import Servant          (ServantErr)

import Futurice.App.FutuhoursApi.Ctx
import Futurice.App.FutuhoursApi.Types

import qualified FUM

projectEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> ExceptT ServantErr IO (Vector Project)
projectEndpoint = error "projectEndpoint: implement me"

-- | @GET /user@
userEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> ExceptT ServantErr IO User
userEndpoint = error "userEndpoint: implement me"

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> ExceptT ServantErr IO HoursResponse
hoursEndpoint = error "hoursEndpoint: implement me"

-- | @POST /entry@
entryEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryEndpoint = error "entryEndpoint: implement me"

-- | @PUT /entry/#id@
entryIdEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryIdEndpoint = error "entryIdEndpoint: implement me"

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryDeleteEndpoint = error "entryDeleteEndpoint: implement me"
