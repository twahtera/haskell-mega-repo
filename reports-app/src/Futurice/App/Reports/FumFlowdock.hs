{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | FUM
module Futurice.App.Reports.FumFlowdock (
    -- * Report
    FumFlowdockReport,
    fumFlowdockReport,
    -- * Types
    FlowdockUser (..),
    FUMUser (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Arrow           ((&&&))
import Control.Lens            (to)
import Data.List               (partition)
import Data.Swagger            (NamedSchema (..))
import Futurice.Generics
import Futurice.Integrations
import Futurice.Report.Columns

import qualified Chat.Flowdock.REST as FD
import qualified Data.Vector        as V
import qualified FUM

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data FlowdockUser = FlowdockUser
    { _fdUserName  :: !Text
    , _fdNick      :: !Text
    , _fdUserId    :: !FD.UserId
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

data FUMUser = FUMUser
    { _fumUserName    :: !Text
    , _fumUserLogin   :: !Text
    , _fumFlowdockUid :: !(Maybe FD.UserId)
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

--makeLenses ''FlowdockUser
--makeLenses ''FUMUser

deriveGeneric ''FlowdockUser
deriveGeneric ''FUMUser

instance NFData FlowdockUser
instance NFData FUMUser

instance ToJSON FlowdockUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToJSON FUMUser where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance ToSchema FlowdockUser where
    declareNamedSchema = sopDeclareNamedSchema
instance ToSchema FUMUser where
    declareNamedSchema = sopDeclareNamedSchema

instance ToColumns FlowdockUser where
    columnNames _ =
        K "fd-name" :*
        K "fd-nick" :*
        K "fd-uid":*
        Nil

instance ToColumns FUMUser where
    columnNames _ =
        K "fum-name" :*
        K "fum-login" :*
        K "fum-flowdock-uid" :*
        Nil

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type FumFlowdockReport = Report
    "Users in FUM ↔ Flowdock"
    ReportGenerated
    (Vector (These FlowdockUser FUMUser))

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

fumFlowdockReport
    :: forall m env.
        ( MonadTime m, MonadFUM m, MonadFlowdock m
        , MonadReader env m, HasFUMEmployeeListName env, HasFlowdockOrgName env
        )
    => m FumFlowdockReport
fumFlowdockReport = do
    now <- currentTime
    fs <- fumEmployeeList
    fds <- view FD.orgUsers <$> flowdockOrganisation
    return $ Report (ReportGenerated now) $ makeReport fds fs

  where
    fumKey :: FUM.User -> Key
    fumKey u = Key
        (u ^? FUM.userFlowdock . lazy . _Just . to (FD.mkIdentifier . fromIntegral))
        (u ^.FUM.userFirst <> " " <> u ^. FUM.userLast)
        (fromMaybe ((u ^. FUM.userName . FUM.getUserName) <> "@futurice.com") $ u ^. FUM.userEmail . lazy)

    fdKey :: FD.OrgUser -> Key
    fdKey u = Key (Just $ u ^. FD.userId) (u ^. FD.userName) (u ^. FD.userEmail)

    mkFum :: FUM.User -> FUMUser
    mkFum u = FUMUser
        { _fumUserName    = u ^. FUM.userFirst <> " " <> u ^. FUM.userLast
        , _fumUserLogin   = u ^. FUM.userName ^. FUM.getUserName
        , _fumFlowdockUid = u ^? FUM.userFlowdock . lazy . _Just . to (FD.mkIdentifier . fromIntegral)
        }

    mkFD :: FD.OrgUser -> FlowdockUser
    mkFD u = FlowdockUser
        { _fdUserName = u ^. FD.userName
        , _fdNick     = u ^. FD.userNick
        , _fdUserId   = u ^. FD.userId
        }

    makeReport
        :: Vector FD.OrgUser -> Vector FUM.User
        -> Vector (These FlowdockUser FUMUser)
    makeReport gs fs =
        let gs' = map (fdKey &&& mkFD) . toList $ gs
            fs' = map (fumKey &&& mkFum) . toList $ fs
            hm  = alignByKey gs' fs'
        in V.fromList . sort $ hm

-------------------------------------------------------------------------------
-- Auxiliary type to align users
-------------------------------------------------------------------------------

-- | user-id, email, name
data Key = Key (Maybe FD.UserId) Text Text
  deriving (Show)

-- | This is not actual equality comparison, as it's not transitive.
keyEq :: Key -> Key -> Bool
keyEq (Key a b c) (Key a' b' c')
    = fromMaybe False ((==) <$> a <*> a')
    || b == b'
    || c == c'

alignByKey :: [(Key, a)] -> [(Key, b)] -> [These a b]
alignByKey [] ys       = map (That . snd) ys
alignByKey ((k, x) : xs) ys = case as of
    [] -> This x : rest
    _  -> map (These x . snd) as ++ rest
  where
    (as, bs) = partition (keyEq k . fst) ys
    rest     = alignByKey xs bs

-------------------------------------------------------------------------------
-- Flowdock orphans
-------------------------------------------------------------------------------

instance ToSchema (FD.Identifier a res) where
    declareNamedSchema _ = return $ NamedSchema (Just "identifier") mempty
