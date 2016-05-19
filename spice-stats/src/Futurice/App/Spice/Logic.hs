{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Spice.Logic (
    Stats(..),
    spiceStats,
    fetchMessagesLoop,
    contrSubject,
    contrHours,
    repoDescription,
    repoLanguage,
    repoOwner,
    repoName,
    repoStarsCount,
    statsContributions,
    statsContributors,
    statsGithubRepos,
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Lens        (Fold, Traversal', failing, folded, folding,
                            (^..), _2, _Just)
import Data.Char           (isAlphaNum, isDigit, isSpace)
import Network.HTTP.Client (Manager)

import Text.Regex.Applicative.Text (RE', psym, sym)

import qualified Chat.Flowdock.REST          as FD
import qualified Chat.Flowdock.REST.IO       as FDIO
import qualified Data.List                   as L
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified GitHub                      as GH
import qualified Text.Regex.Applicative.Text as RE

import Futurice.App.Spice.Types

mailMessageOpts :: FD.MessageOptions
mailMessageOpts =
    FD.defMessageOptions
        & FD.msgOptEvents .~ [FD.EventDiscussion]
        & FD.msgOptLimit  .~ Just 100

fetchMessagesLoop :: FD.ParamName FD.Organisation
                  -> FD.ParamName FD.Flow
                  -> FD.AuthToken
                  -> Manager
                  -> IO [FD.Message]
fetchMessagesLoop org flow token mgr = go Nothing
  where
    go :: Maybe FD.MessageId -> IO [FD.Message]
    go mMsgId = do
        let msgOpts = mailMessageOpts & FD.msgOptUntilId .~ mMsgId
        msgs <- FDIO.messages mgr token org flow msgOpts
        case msgs of
            []    -> return []
            (x:_) -> (<> msgs) <$> go (Just $ x ^. FD.msgId)

digittt :: RE' Char
digittt = psym isDigit

floatRe :: RE' Float
floatRe = f <$> some digittt <*> optional (dot *> some digittt)
  where dot = (sym ',' <|> sym '.')
        f xs ys = read (xs <> "." <> fromMaybe "0" ys)

hoursRe :: RE' Float
hoursRe = floatRe <* many (psym isSpace) <* (RE.string "h" <|> RE.string "H")

githubRe :: RE' (GH.Name GH.Owner, GH.Name GH.Repo)
githubRe = mk <$ RE.string "github.com/" <*> some (RE.psym p) <* RE.sym '/' <*> some (RE.psym p)
  where p c = isAlphaNum c || c `L.elem` ("-_." :: String)
        mk owner repo = (GH.mkOwnerName $ T.pack owner, GH.mkRepoName $ T.pack repo)

extractContribution :: FD.Discussion -> Maybe Contribution
extractContribution discussion = constructor $ Contribution
    (fromMaybe 0 hours)
    (discussion ^. FD.discussionAuthor)
    subject
    github
  where
    constructor
        | discussion ^. FD.discussionThread
                      . FD.threadSource
                      . FD.sourceApplication
                      . FD.applicationName == "Email" = Just
        | otherwise = const Nothing
    subject = discussion ^. FD.discussionTitle
    content = discussion ^. FD.discussionBody
    github = (^. _2) <$> (RE.findFirstInfix githubRe content)
    hours = (^. _2) <$> (RE.findFirstInfix hoursRe subject  <|> RE.findFirstInfix hoursRe content)

discussionContribution :: Fold FD.Discussion Contribution
discussionContribution = folding extractContribution

authorNameOrEmail :: Traversal' FD.Author Text
authorNameOrEmail = FD.authorName . _Just `failing` FD.authorEmail . _Just

spiceStats :: Manager -> [FD.Message] -> GH.Auth -> IO Stats
spiceStats mgr msgs auth =
    Stats butions butors <$> repos
  where
    cs :: [Contribution]
    cs = msgs ^.. traverse . FD.msgContent . FD._MTDiscussion . discussionContribution

    butions   = V.fromList $ cs ^.. folded
    butors    = V.fromList . L.sort . L.nub $ butions ^.. traverse . contrAuthor . authorNameOrEmail
    repoNames = V.fromList . L.sort . L.nub $ butions ^.. traverse . contrGithub . _Just

    repos :: IO (Vector SpiceRepo)
    repos = V.fromList . (\l -> l ^.. folded . _Just) <$> traverse (uncurry spiceRepo) repoNames

    spiceRepo :: GH.Name GH.Owner -> GH.Name GH.Repo -> IO (Maybe SpiceRepo)
    spiceRepo owner repo = either (const Nothing) (Just . mkSpiceRepo) <$>
        GH.executeRequestWithMgr mgr auth (GH.repositoryR owner repo)

    mkSpiceRepo :: GH.Repo -> SpiceRepo
    mkSpiceRepo repo = SpiceRepo
        (GH.simpleOwnerLogin . GH.repoOwner $ repo)
        (GH.repoName repo)
        (GH.repoDescription repo)
        (GH.repoStargazersCount repo)
        (GH.repoLanguage repo)
