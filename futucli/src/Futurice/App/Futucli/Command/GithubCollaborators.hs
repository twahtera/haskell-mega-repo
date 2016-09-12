{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Futucli.Command.GithubCollaborators (githubCollaborators) where

import Futurice.Prelude

import Control.Monad.Reader    (ReaderT (..))
import GHC.Exts                (Constraint)
import Network.HTTP.Client     (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO               (stderr)

import qualified Data.Binary                 as B
import qualified Data.ByteString.Base64.Lazy as Base64L
import qualified Data.ByteString.Lazy.Char8  as BSL8
import qualified Data.Text.IO                as T
import qualified Data.Vector                 as V

import qualified GitHub as GH

import Futurice.App.Futucli.Cfg

-- | TODO: move to own file
class (Applicative m, Monad m) => MonadGithub m where
    type MonadGithubC m a :: Constraint
    type MonadGithubC m a = ()

    githubReq :: MonadGithubC m a => GH.Request 'False a -> m a

data GithubCfg = GithubCfg !Manager !GH.Auth

class HasGithubCfg env where
    githubCfg :: Lens' env GithubCfg

instance HasGithubCfg GithubCfg where
    githubCfg = id

instance (Applicative m, MonadIO m, MonadThrow m, HasGithubCfg env) => MonadGithub (ReaderT env m) where
    type MonadGithubC (ReaderT env m) a = Binary a

    githubReq r = ReaderT $ \env -> do
        d <- liftIO $ B.decodeFileOrFail path `catch` handleIOError (pure $ Left undefined)
        case d of
            Right x  -> pure x
            Left _   -> case env ^. githubCfg of
                GithubCfg mgr auth -> do
                    x <- executeRequestWithMgr mgr auth r
                    liftIO $ B.encodeFile path x
                    pure x
      where
        path = ".cache/" ++ encodeR r

handleIOError :: a -> IOError -> a
handleIOError = const

encodeR :: GH.Request k a -> String
encodeR = BSL8.unpack . Base64L.encode . B.encode

evalReaderMonadGithub' :: Manager -> GH.Auth -> ReaderT GithubCfg IO a -> IO a
evalReaderMonadGithub' mgr auth m =
    runReaderT m (GithubCfg mgr auth)

evalReaderMonadGithub :: Cfg -> ReaderT GithubCfg IO a -> IO a
evalReaderMonadGithub cfg m = do
    mgr <- newManager tlsManagerSettings
    let auth = _cfgGhToken cfg
    evalReaderMonadGithub' mgr auth m

-- TODO: use MonadLogger
githubCollaborators :: Cfg -> IO ()
githubCollaborators cfg = evalReaderMonadGithub cfg $ do
    let org = _cfgGhOrg cfg
    let owner =  GH.fromOrganizationName org
    repos <- fmap (sort . fmap GH.repoName . V.toList) $ githubReq $ GH.organizationReposR org GH.RepoPublicityAll GH.FetchAll
    pairs <- concat <$> traverse (\repoName -> fmap (,repoName) <$> fetchCollaborators owner repoName) repos
    members <- githubReq $ GH.membersOfWithR org GH.OrgMemberFilterAll GH.OrgMemberRoleAll GH.FetchAll
    let pairs' = sort $ filter (\p -> notElem (fst p) members) pairs
    traverse_ printPair pairs'

printPair
    :: (MonadIO m, MonadGithub m, MonadGithubC m GH.User)
    => (GH.SimpleUser, GH.Name GH.Repo) -> m ()
printPair (u', r) = do
  u <- githubReq $ GH.userInfoForR (GH.simpleUserLogin u')
  liftIO $ T.putStrLn $ mconcat
      [ GH.untagName (GH.userLogin u)
      , ": "
      , fromMaybe "" (GH.userName u)
      , " => "
      , GH.untagName r
      ]

fetchCollaborators
    :: (MonadGithub m, MonadIO m, MonadGithubC m (Vector GH.SimpleUser))
    => GH.Name GH.Owner -> GH.Name GH.Repo -> m [GH.SimpleUser]
fetchCollaborators owner repoName = do
    liftIO $ T.hPutStrLn stderr $ "Repo: " <> GH.untagName repoName
    fmap V.toList $ githubReq $ GH.collaboratorsOnR owner repoName GH.FetchAll

executeRequestWithMgr :: (MonadIO m, MonadThrow m) => Manager -> GH.Auth -> GH.Request k a -> m a
executeRequestWithMgr mgr auth r = do
    x <- liftIO $ GH.executeRequestWithMgr mgr auth r
    either throwM return x
