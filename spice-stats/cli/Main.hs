module Main (main, contrSubject, repoDescription, repoLanguage, repoOwner, repoName, repoStarsCount, statsGithubRepos) where

import Prelude ()
import Futurice.Prelude
import Control.Lens       (sumOf)
import Futurice.EnvConfig (getConfig)

import Futurice.App.Spice.Config
import Futurice.App.Spice.Logic

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty hiding (dot)

printStats :: Stats -> IO ()
printStats s = do
    let butions = s ^. statsContributions
    let butors = s ^. statsContributors
    putDoc $ ansiPretty s <> linebreak
    putDoc $ text "Contributions in total" <+> pretty (length butions) <> linebreak
    putDoc $ text "Unique contributors" <+> pretty (length butors) <> linebreak
    putDoc $ text "Hours reported" <+> pretty (sumOf (folded . contrHours) butions) <> linebreak

main :: IO ()
main = withStderrLogger $ \logger -> do
    cfg <- getConfig logger "SPICESTATS"
    mgr <- newManager tlsManagerSettings
    msgs <- fetchMessagesLoop (cfgFdOrg cfg) (cfgFdFlow cfg) (cfgFdAuth cfg) mgr
    s <- spiceStats mgr msgs (cfgGhAuth cfg)
    printStats s
