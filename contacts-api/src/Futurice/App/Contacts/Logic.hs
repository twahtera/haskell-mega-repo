{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
module Futurice.App.Contacts.Logic (
    contacts,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Maybe            (mapMaybe)
import Data.RFC5051          (compareUnicode)
import Futurice.Integrations

import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict   as S
import qualified Data.Text           as T
import qualified Data.Vector         as V

-- Data definition
import qualified Chat.Flowdock.REST as FD
import qualified FUM
import qualified GitHub             as GH
import qualified PlanMill           as PM
import qualified PlanMill.Queries   as PMQ

-- Contacts modules
import Futurice.App.Contacts.Types
import Futurice.App.Contacts.Types.Tri (lessSure)

compareUnicodeText :: Text -> Text -> Ordering
compareUnicodeText = compareUnicode `on` T.unpack

-- | Get contacts data
contacts
    :: ( MonadFlowdock m, MonadGitHub m, MonadFUM m, MonadPlanMillQuery m
       , MonadReader env m
       , HasGithubOrgName env, HasFUMEmployeeListName env, HasFlowdockOrgName env
       )
    => m [Contact Text]
contacts = contacts'
    <$> fumEmployeeList
    <*> githubDetailedMembers
    <*> flowdockOrganisation
    <*> fumPlanmillTeamMap

-- | The pure, data mangling part of 'contacts'
contacts'
    :: Vector FUM.User
    -> Vector GH.User
    -> FD.Organisation
    -> HashMap FUM.UserName (Maybe Text)
    -> [Contact Text]
contacts' users githubMembers flowdockOrg teamMap =
    let users' = filter ((==FUM.StatusActive) . view FUM.userStatus) $ V.toList users
        res0 = map userToContact users'
        res1 = addGithubInfo githubMembers res0
        res2 = addFlowdockInfo (flowdockOrg ^. FD.orgUsers) res1
        res3 = addPlanmillInfo teamMap res2
    in sortBy (compareUnicodeText `on` contactName) res3

userToContact :: FUM.User -> Contact Text
userToContact FUM.User{..} = Contact
    { contactLogin    = _userName
    , contactFirst    = _userFirst
    , contactName     = _userFirst <> " " <> _userLast
    , contactEmail    = S.fromMaybe defaultEmail _userEmail
    , contactPhones   = S.catMaybes [_userPhone1, _userPhone2]
    , contactTitle    = _userTitle ^. lazy
    , contactThumb    = S.fromMaybe noImage _userThumbUrl
    , contactImage    = S.fromMaybe noImage _userImageUrl
    , contactFlowdock = S.maybe Unknown (Sure . (\uid -> ContactFD uid "-" noImage)) _userFlowdock
    , contactGithub   = S.maybe Unknown (Sure . flip ContactGH noImage) _userGithub
    , contactTeam     = Nothing
    }
  where
    noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"
    defaultEmail = FUM._getUserName _userName <> "@futurice.com"

githubDetailedMembers
    :: ( MonadGitHub m
       , MonadReader env m, HasGithubOrgName env
       )
    => m (Vector GH.User)
githubDetailedMembers = do
    githubMembers <- githubOrganisationMembers
    traverse (githubReq . GH.userInfoForR . GH.simpleUserLogin) githubMembers

fumPlanmillTeamMap
    :: forall m env.
       ( MonadPlanMillQuery m, MonadFUM m
       , MonadReader env m, HasFUMEmployeeListName env
       )
    => m (HashMap FUM.UserName (Maybe Text))
fumPlanmillTeamMap = fumPlanmillMap >>= traverse f
  where
    f :: PM.User -> m (Maybe Text)
    f u = PM.tName <$$> traverse PMQ.team (PM.uTeam u)


addGithubInfo
    :: (Functor f, Foldable g)
    => g GH.User -> f (Contact Text) -> f (Contact Text)
addGithubInfo gh = fmap add
  where
    gh' = toList gh

    nameMap :: HM.HashMap Text GH.User
    nameMap = HM.fromList (mapMaybe pair gh')
      where
        pair :: GH.User -> Maybe (Text, GH.User)
        pair x = (\y -> (y, x)) <$> GH.userName x

    loginMap :: HM.HashMap Text GH.User
    loginMap = HM.fromList (map pair gh')
      where
        pair :: GH.User -> (Text, GH.User)
        pair x = (GH.untagName $ GH.userLogin x, x)

    add :: Contact Text -> Contact Text
    add c = c{ contactGithub = (cgh >>= byLogin . cghNick)
                             <> lessSure cgh
                             <> byName }
      where
        cgh :: Tri (ContactGH Text)
        cgh = contactGithub c

        byName :: Tri (ContactGH Text)
        byName = maybe Unknown
                       (Unsure . fromDetailedOwner)
                       (HM.lookup (contactName c) nameMap)

        byLogin :: Text -> Tri (ContactGH Text)
        byLogin ghLogin = maybe Unknown
                                (Sure . fromDetailedOwner)
                                (HM.lookup ghLogin loginMap)

fromDetailedOwner :: GH.User -> ContactGH Text
fromDetailedOwner gh = ContactGH
    { cghNick   = GH.untagName . GH.userLogin $ gh
    , cghAvatar = GH.getUrl $ GH.userAvatarUrl gh
    }

addFlowdockInfo
    :: forall u f g. (FD.UserLike u, Functor f, Foldable g)
    => g u
    -> f (Contact Text)
    -> f (Contact Text)
addFlowdockInfo us = fmap add
  where
    -- we could use ixsed-typed for these
    emailMap :: HM.HashMap Text u
    emailMap = foldMap (\u -> HM.singleton (u ^. FD.userEmail) u) us

    nameMap :: HM.HashMap Text u
    nameMap = foldMap (\u -> HM.singleton (u ^. FD.userName) u) us

    uidMap :: HM.HashMap FD.UserId u
    uidMap = foldMap (\u -> HM.singleton (u ^. FD.userId) u) us

    add :: Contact Text -> Contact Text
    add c = c
        { contactFlowdock =
            (cfd >>= byId . FD.mkIdentifier . fromIntegral . cfdId)
            <> lessSure cfd
            <> byEmail
            <> byName
        }
      where
        cfd = contactFlowdock c
        name = contactName c
        email = contactEmail c

        byId uid = maybe Unknown (Sure . f) (HM.lookup uid uidMap)
        byEmail = maybe Unknown (Sure . f)   (HM.lookup email emailMap)
        byName  = maybe Unknown (Unsure . f) (HM.lookup name nameMap)

        f :: u -> ContactFD Text
        f u = ContactFD (fromInteger $ FD.getIdentifier $ u ^. FD.userId)
                        (u ^. FD.userNick)
                        (u ^. FD.userAvatar)

addPlanmillInfo
    :: Functor f
    => HashMap FUM.UserName (Maybe Text)
    -> f (Contact a)
    -> f (Contact a)
addPlanmillInfo teamMap = fmap f
  where
    f :: Contact a -> Contact a
    f c = c
        { contactTeam = join $ HM.lookup (contactLogin c) teamMap
        }
