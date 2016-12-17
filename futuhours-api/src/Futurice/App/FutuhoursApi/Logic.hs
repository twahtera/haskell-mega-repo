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

import Futurice.App.FutuhoursApi.Types

projectEndpoint :: Ctx -> IO (Vector Project)
projectEndpoint = error "projectEndpoint: implement me"

-- | @GET /user@
userEndpoint :: Ctx -> IO User
userEndpoint = error "userEndpoint: implement me"

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe Day
    -> Maybe Day
    -> ExceptT ServantErr IO HoursResponse
hoursEndpoint = error "hoursEndpoint: implement me"

-- | @POST /entry@
entryEndpoint
    :: Ctx
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryEndpoint = error "entryEndpoint: implement me"

-- | @PUT /entry/#id@
entryIdEndpoint
  :: Ctx
  -> Int
  -> EntryUpdate
  -> ExceptT ServantErr IO EntryUpdateResponse
entryIdEndpoint = error "entryIdEndpoint: implement me"

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Int
    -> EntryUpdate
    -> ExceptT ServantErr IO EntryUpdateResponse
entryDeleteEndpoint = error "entryDeleteEndpoint: implement me"
