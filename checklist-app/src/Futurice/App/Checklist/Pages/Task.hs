{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Task (taskPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (re)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified FUM (UserName (..))

-- |
--
-- === Preconditions
--
-- * 'Task' is in the 'World'.
taskPage
    :: World
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Task
    -> HtmlPage "task"
taskPage _world authUser task = page_ (view nameText task <> " - Checklist") pageParams $ do
    navigation authUser

    -- Title
    header (task ^. nameText) []

    -- Edit
    row_ $ large_ 12 $ form_ $ do
        row_ $ large_ 12 $
            label_ $ do
                "Name"
                input_ [ type_ "text", value_ $ (task ^. nameText) ]
        row_ $ large_ 12 $
            label_ $ do
                "Role"
                select_ [] $ for_ [ minBound .. maxBound ] $ \role ->
                    optionSelected_ (role == task ^. taskRole)
                        [ value_ $ role ^. re _TaskRole ]
                        $ toHtml $ roleToText role

        row_ $ large_ 12 $ div_ [ class_ "button-group" ] $ do
            button_ [ class_ "button success" ] $ "Save"
            button_ [ class_ "button" ] $ "Reset"
