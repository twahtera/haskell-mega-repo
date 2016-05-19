{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}
module Futurice.App.Contacts.Logic (
    contactsAction,
    ) where

import Futurice.Prelude
import Prelude          ()

import Data.Maybe    (mapMaybe)
import Data.RFC5051  (compareUnicode)
import Haxl.Typed

import qualified Data.HashMap.Strict as HM
import qualified Data.Maybe.Strict   as S
import qualified Data.Text           as T
import qualified Data.Vector         as V

-- Data definition
import qualified Chat.Flowdock.REST          as FD
import qualified Chat.Flowdock.REST.Internal as FD
import qualified FUM
import qualified GitHub                      as GH

-- Haxl data sources
import qualified Flowdock.TyHaxl           as FDDataSource
import qualified FUM.TyHaxl                as FUMDataSource
import qualified Github.TyHaxl             as GHDataSource
import qualified Haxl.Typed.HttpDataSource as HttpDataSource
import qualified Haxl.Typed.LogDataSource  as LogDataSource

-- Contacts modules
import Futurice.App.Contacts.Types
import Futurice.App.Contacts.Types.Tri (lessSure)

compareUnicodeText :: Text -> Text -> Ordering
compareUnicodeText = compareUnicode `on` T.unpack

type In e es = IsElem' e es (Index e es)
type AllIn es es' = IsSubset' es es' (Image es es')

type Effects = '[ FUMDataSource.FumRequest
                , FDDataSource.FlowdockRequest
                , GHDataSource.GithubRequest
                , HttpDataSource.HttpRequest
                ]

contactsAction :: GH.Name GH.Organization       -- ^ Github organisation
               -> FD.ParamName FD.Organisation  -- ^ Flowdock organistion
               -> FUM.ListName                  -- ^ FUM user list
               -> FUM.AuthToken                 -- ^ FUM access token
               -> FUM.BaseUrl                   -- ^ FUM base url
               -> GH.Auth                       -- ^ Github access token
               -> FD.AuthToken                  -- ^ Flowdock access token
               -> IO [Contact Text]
contactsAction ghOrg fdOrg fumUserList = runHaxl' haxl
  where
    haxl = contactsHaxl ghOrg fdOrg fumUserList ::
              GenTyHaxl Effects () [Contact Text]

runHaxl' :: forall a r.
            AllIn r '[ FUMDataSource.FumRequest
                     , GHDataSource.GithubRequest
                     , FDDataSource.FlowdockRequest
                     , HttpDataSource.HttpRequest
                     , LogDataSource.LogRequest
                     ]
         => GenTyHaxl r () a
         -> FUM.AuthToken
         -> FUM.BaseUrl
         -> GH.Auth
         -> FD.AuthToken
         -> IO a
runHaxl' haxl token url ghAuth fdAuth = do
    fumDS <- FUMDataSource.initDataSource token url
    ghDS <- GHDataSource.initDataSource ghAuth
    fdDS <- FDDataSource.initDataSource fdAuth
    httpDS <- HttpDataSource.initDataSource
    logDS <- LogDataSource.initDataSource
    environment <- initTyEnv (fumDS  `tyStateSet`
                              ghDS   `tyStateSet`
                              fdDS   `tyStateSet`
                              httpDS `tyStateSet`
                              logDS  `tyStateSet` tyStateEmpty) ()
    runTyHaxl environment haxl

--

userToContact :: FUM.User -> Contact Text
userToContact FUM.User{..} = Contact
    (FUM._getUserName _userName)
    _userFirst
    (_userFirst <> " " <> _userLast)
    (S.fromMaybe defaultEmail _userEmail)
    (S.catMaybes [_userPhone1, _userPhone2])
    (view lazy _userTitle)
    (S.fromMaybe noImage _userThumbUrl)
    (S.fromMaybe noImage _userImageUrl)
    (S.maybe Unknown (Sure . (\uid -> ContactFD uid "-" noImage)) _userFlowdock)
    (S.maybe Unknown (Sure . flip ContactGH noImage) _userGithub)
  where
    noImage = "https://avatars0.githubusercontent.com/u/852157?v=3&s=30"
    defaultEmail = FUM._getUserName _userName <> "@futurice.com"

githubDetailedMembersOf
    :: In GHDataSource.GithubRequest r
    => GH.Name GH.Organization
    -> GenTyHaxl r u [GH.User]
githubDetailedMembersOf org = do
    githubMembers <- GHDataSource.membersOf org
    traverse (GHDataSource.userInfoFor . GH.simpleUserLogin) (V.toList githubMembers)

addGithubInfo :: [GH.User] -> [Contact Text] -> [Contact Text]
addGithubInfo gh = fmap add
  where
    nameMap :: HM.HashMap Text GH.User
    nameMap = HM.fromList (mapMaybe pair gh)
      where
        pair :: GH.User -> Maybe (Text, GH.User)
        pair x = (\y -> (y, x)) <$> GH.userName x

    loginMap :: HM.HashMap Text GH.User
    loginMap = HM.fromList (map pair gh)
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
fromDetailedOwner gh = ContactGH (GH.untagName . GH.userLogin $ gh)
                                 (GH.userAvatarUrl gh)

addFlowdockInfo
    :: forall u f. (FD.UserLike u, Functor f)
    => Vector u
    -> f (Contact Text)
    -> f (Contact Text)
addFlowdockInfo us = fmap add
  where
    emailMap :: HM.HashMap Text u
    emailMap = foldMap (\u -> HM.singleton (u ^. FD.userEmail) u) us

    nameMap :: HM.HashMap Text u
    nameMap = foldMap (\u -> HM.singleton (u ^. FD.userName) u) us

    uidMap :: HM.HashMap FD.UserId u
    uidMap = foldMap (\u -> HM.singleton (u ^. FD.userId) u) us

    add :: Contact Text -> Contact Text
    add c = c{ contactFlowdock = (cfd >>= byId . FD.mkIdentifier . fromIntegral . cfdId)
                               <> lessSure cfd
                               <> byEmail
                               <> byName }
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

contactsHaxl
    :: ( In FUMDataSource.FumRequest r
       , In FDDataSource.FlowdockRequest r
       , In GHDataSource.GithubRequest r
       , In HttpDataSource.HttpRequest r
       )
    => GH.Name GH.Organization
    -> FD.ParamName FD.Organisation
    -> FUM.ListName
    -> GenTyHaxl r u [Contact Text]
contactsHaxl ghOrg fdOrg fumUserList = contactsHaxl'
    <$> FUMDataSource.fetchList fumUserList
    <*> githubDetailedMembersOf ghOrg
    <*> FDDataSource.organisation fdOrg

contactsHaxl' :: Vector FUM.User
              -> [GH.User]
              -> FD.Organisation
              -> [Contact Text]
contactsHaxl' users githubMembers flowdockOrg =
    let users' = filter ((==FUM.StatusActive) . view FUM.userStatus) $ V.toList users
        contacts = map userToContact users'
        contacts' = addGithubInfo githubMembers contacts
        contacts'' = addFlowdockInfo (flowdockOrg ^. FD.orgUsers) contacts'
    in sortBy (compareUnicodeText `on` contactName) contacts''
