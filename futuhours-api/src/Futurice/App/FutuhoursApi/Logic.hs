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
import Control.Concurrent.STM (readTVarIO)
import Servant                (Handler, err403)

import Futurice.App.FutuhoursApi.Ctx
import Futurice.App.FutuhoursApi.Types

import qualified FUM
import qualified PlanMill as PM

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

projectEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler (Vector Project)
projectEndpoint = error "projectEndpoint: implement me"

-- | @GET /user@
userEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Handler User
userEndpoint ctx mfum = authorisedUser ctx mfum $ \fumUsername _pmUser ->
    error $ "userEndpoint: implement me " ++ show fumUsername

-- | @GET /hours@
hoursEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Maybe Day
    -> Maybe Day
    -> Handler HoursResponse
hoursEndpoint = error "hoursEndpoint: implement me"

-- | @POST /entry@
entryEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryEndpoint = error "entryEndpoint: implement me"

-- | @PUT /entry/#id@
entryIdEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryIdEndpoint = error "entryIdEndpoint: implement me"

-- | @DELETE /entry/#id@
entryDeleteEndpoint
    :: Ctx
    -> Maybe FUM.UserName
    -> Int
    -> EntryUpdate
    -> Handler EntryUpdateResponse
entryDeleteEndpoint = error "entryDeleteEndpoint: implement me"

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

authorisedUser
    :: Ctx -> Maybe FUM.UserName
    -> (FUM.UserName -> PM.User -> Handler a)
    -> Handler a
authorisedUser ctx mfum f =
    mcase (mfum <|> ctxMockUser ctx) (throwError err403) $ \fumUsername -> do
        userMap <- liftIO $ readTVarIO $ ctxPlanmillUserLookup ctx
        pmUser <- maybe (throwError err403) pure $ userMap ^. at fumUsername
        f fumUsername pmUser
