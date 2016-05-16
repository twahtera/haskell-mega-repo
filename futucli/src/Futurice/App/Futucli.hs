{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
module Futurice.App.Futucli (defaultMain) where

import Prelude        ()
import Prelude.Compat

import Control.Monad.Catch (Exception, throwM)
import Data.Maybe          (fromMaybe)
import Data.Monoid         ((<>))
import Data.Typeable       (Typeable)
import Data.Yaml
import System.Environment  (lookupEnv)

import qualified Options.Applicative as O

import Futurice.App.Futucli.Cfg
import Futurice.App.Futucli.Command.FumGithubCheck
import Futurice.App.Futucli.Command.GeneratePassword
import Futurice.App.Futucli.Command.GithubCollaborators
import Futurice.App.Futucli.Command.GithubMembersNo2FA
import Futurice.App.Futucli.Command.GithubMembersNoTeam
import Futurice.App.Futucli.Command.GithubOldRepos
import Futurice.App.Futucli.Command.PlanMillUserIds

newtype MissingEnvVarException = MissingEnvVarException String
    deriving (Show, Typeable)

instance Exception MissingEnvVarException

lookupEnvVar :: String -> IO String
lookupEnvVar envvar = fromMaybe (throwM $ MissingEnvVarException envvar) <$> lookupEnv envvar

data Cmd
    = PlanMillUserIds
    | FumGithubCheck
    | GithubMembersNo2FA
    | GithubMembersNoTeam
    | GithubCollaborators
    | GithubOldRepos Integer Int
    | GeneratePassword Int
    deriving Show

planmillUserIdsOptions :: O.Parser Cmd
planmillUserIdsOptions = pure PlanMillUserIds

fumGithubCheckOptions :: O.Parser Cmd
fumGithubCheckOptions = pure FumGithubCheck

githubMembersNo2FAOptions :: O.Parser Cmd
githubMembersNo2FAOptions = pure GithubMembersNo2FA

githubMembersNoTeamOptions :: O.Parser Cmd
githubMembersNoTeamOptions = pure GithubMembersNoTeam

githubCollaboratorsOptions :: O.Parser Cmd
githubCollaboratorsOptions = pure GithubCollaborators

githubOldReposOptions :: O.Parser Cmd
githubOldReposOptions = GithubOldRepos
    <$> O.option O.auto (O.long "days" <> O.metavar ":days" <> O.help "How old repositories to list" <> O.value 365 <> O.showDefault)
    <*> O.option O.auto (O.long "commits" <> O.metavar ":commits" <> O.help "How small repositories to list" <> O.value 5 <> O.showDefault)

generatePasswordOptions :: O.Parser Cmd
generatePasswordOptions = GeneratePassword
    <$> O.option O.auto (O.long "chars" <> O.metavar ":chars" <> O.help "Characters" <> O.value 32 <> O.showDefault)

optsParser :: O.Parser Cmd
optsParser = O.subparser $ mconcat
    [ cmdParser "planmill-user-ids" planmillUserIdsOptions "List planmill user ids"
    , cmdParser "fum-github-check" fumGithubCheckOptions "List users in FUM but not in Github, and vice-versa"
    , cmdParser "github-members-no-2fa"githubMembersNo2FAOptions "List users in Github organisation with 2fa disabled"
    , cmdParser "github-members-no-team" githubMembersNoTeamOptions "List users in Github organisation without any team"
    , cmdParser "github-collaborators" githubCollaboratorsOptions "List collaborators and repositories"
    , cmdParser "github-old-repos" githubOldReposOptions "List old github repositories"
    , cmdParser "generate-password" generatePasswordOptions "Generate random password"
    ]
  where
    cmdParser :: String -> O.Parser Cmd -> String -> O.Mod O.CommandFields Cmd
    cmdParser cmd parser desc =
         O.command cmd $ O.info (O.helper <*> parser) $ O.progDesc desc

main' :: Cmd -> Cfg -> IO ()
main' PlanMillUserIds     = planMillUserIds
main' FumGithubCheck      = fumGithubCheck
main' GithubMembersNo2FA  = githubMembersNo2FA
main' GithubMembersNoTeam = githubMembersNoTeam
main' GithubCollaborators = githubCollaborators
main' (GithubOldRepos days commits) =
    \cfg ->  githubOldRepos cfg days commits
main' (GeneratePassword l) = const (generatePassword l)

defaultMain :: IO ()
defaultMain = do
    homeVar <- lookupEnvVar "HOME"
    cfg <- either throwM pure =<< decodeFileEither (homeVar <> "/.futuclirc")
    O.execParser opts >>= flip main' cfg
  where
    opts = O.info (O.helper <*> optsParser) $ mconcat
        [ O.fullDesc
        , O.progDesc "Futurice cli tools"
        , O.header "futucli"
        ]
