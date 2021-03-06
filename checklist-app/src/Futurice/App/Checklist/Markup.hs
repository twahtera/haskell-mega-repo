{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Markup (
    -- * Structure
    checklistPage_,
    header,
    subheader_,
    -- * Futu id
    futuId_,
    futuForm_,
    -- * Link attributes
    linkToText,
    indexPageHref,
    tasksPageHref,
    checklistsPageHref,
    createChecklistPageHref,
    createTaskPageHref,
    createEmployeePageHref,
    checklistPageHref,
    taskPageHref,
    employeePageHref,
    -- * Links
    employeeLink,
    checklistLink,
    taskLink,
    -- * ToHtml
    nameHtml,
    nameText,
    roleHtml,
    contractTypeHtml,
    checklistNameHtml,
    locationHtml,
    -- * Counter
    TodoCounter (..),
    toTodoCounter,
    -- * Tasks
    taskCheckbox,
    -- * Defaults
    defaultShowAll,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens        (Getter, has, non, only, re, to, _Wrapped)
import Servant.Utils.Links (Link, safeLink)
import Web.HttpApiData     (toUrlPiece)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay  (pageParams)
import Futurice.App.Checklist.Types
import Futurice.Lucid.Foundation

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified FUM

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

checklistPage_ :: Text -> AuthUser -> Html () -> HtmlPage sym
checklistPage_ title authUser body =
    page_ (title <> " - Checklist²" ) pageParams $ do
        navigation authUser
        div_ [ futuId_ "error-callout", class_ "callout alert", style_ "display: none" ] $ do
            div_ [ futuId_ "error-callout-content" ] $ pure ()
            button_ [ class_ "button" ] "Close"
        body

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => AuthUser -> HtmlT m ()
navigation (fu, viewerRole) = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Checklist"
                sup_ "2"
            li_ $ a_ [ id_ "futu-reload-indicator", href_ "#", style_ "display: none", title_ "You made changes, refresh page to show" ]  "1"
            li_ $ a_ [ indexPageHref Nothing (Nothing :: Maybe Checklist) (Nothing :: Maybe Task) defaultShowAll ] "Employees"
            li_ $ a_ [ checklistsPageHref ] "Checklists"
            li_ $ a_ [ tasksPageHref Nothing (Nothing :: Maybe Checklist) ] "Tasks"
            li_ $ a_ [ createChecklistPageHref ] "Create List"
            li_ $ a_ [ createTaskPageHref ] "Create Task"
            li_ $ a_ [ createEmployeePageHref Nothing ] "Create Employee"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "dropdown menu" ] $
            li_ [ class_ "menu-text" ] $ do
                "Hello "
                toHtml $ fu ^. FUM.getUserName
                ", you are "
                toHtml $ viewerRole ^. re _TaskRole

header
    :: Monad m
    => Text          -- ^ default title
    -> [Maybe Text]  -- ^ title parts
    -> HtmlT m ()
header title titleParts' = row_ $ large_ 12 $ header_ $ h1_ $ toHtml $
    if null titleParts
        then title
        else T.intercalate " - " titleParts
  where
    titleParts = catMaybes titleParts'

subheader_
    :: Monad m
    => Text
    -> HtmlT m ()
subheader_ title = row_ $ large_ 12 $ h2_ $ toHtml title

-------------------------------------------------------------------------------
-- Futu id
-------------------------------------------------------------------------------

futuId_ :: Text -> Attribute
futuId_ = data_ "futu-id"

futuForm_ :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
futuForm_ i attrs = row_ . large_ 12 . form_ (futuId_ i : attrs)

-------------------------------------------------------------------------------
-- Name helpers
-------------------------------------------------------------------------------

nameText :: HasName a => Getter a Text
nameText = name . _Wrapped

nameHtml :: (HasName a, Monad m) => Getter a (HtmlT m ())
nameHtml = nameText . to toHtml

-------------------------------------------------------------------------------
-- Hrefs
-------------------------------------------------------------------------------

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

indexPageHref
    :: (HasIdentifier c Checklist, HasIdentifier t Task)
    => Maybe Location -> Maybe c -> Maybe t -> Bool -> Attribute
indexPageHref mloc mlist mtask showAll =
    href_ $ linkToText $ safeLink checklistApi indexPageEndpoint mloc
        (mlist ^? _Just . identifier)
        (mtask ^? _Just . identifier)
        showAll

tasksPageHref
    :: (HasIdentifier c Checklist)
    => Maybe TaskRole -> Maybe c -> Attribute
tasksPageHref mrole mlist =
    href_ $ linkToText $ safeLink checklistApi tasksPageEndpoint mrole
        (mlist ^? _Just . identifier)

checklistsPageHref
    :: Attribute
checklistsPageHref =
    href_ $ linkToText $ safeLink checklistApi checklistsPageEndpoint

createChecklistPageHref :: Attribute
createChecklistPageHref =
    href_ $ linkToText $ safeLink checklistApi createChecklistPageEndpoint

createTaskPageHref :: Attribute
createTaskPageHref =
    href_ $ linkToText $ safeLink checklistApi createTaskPageEndpoint

createEmployeePageHref :: Maybe (Identifier Employee) -> Attribute
createEmployeePageHref meid =
    href_ $ linkToText $ safeLink checklistApi createEmployeePageEndpoint meid

taskPageHref
    :: (HasIdentifier t Task)
    => t
    -> Attribute
taskPageHref t =
    href_ $ linkToText $ safeLink checklistApi taskPageEndpoint
        (t ^. identifier)

employeePageHref
    :: (HasIdentifier c Employee)
    => c
    -> Attribute
employeePageHref e =
    href_ $ linkToText $ safeLink checklistApi employeePageEndpoint
        (e ^. identifier)

checklistPageHref
    :: (HasIdentifier c Checklist)
    => c
    -> Attribute
checklistPageHref l =
    href_ $ linkToText $ safeLink checklistApi checklistPageEndpoint
        (l ^. identifier)

-------------------------------------------------------------------------------
-- Links
-------------------------------------------------------------------------------

employeeLink :: Monad m => Employee -> HtmlT m ()
employeeLink e = a_ [ employeePageHref e ] $ e ^. nameHtml

checklistLink :: Monad m => Checklist -> HtmlT m ()
checklistLink cl = a_ [ checklistPageHref cl ] $ cl ^. nameHtml

taskLink :: Monad m => Task -> HtmlT m ()
taskLink task = a_ [ taskPageHref task ] $ task ^. nameHtml

-------------------------------------------------------------------------------
-- Miscs
-------------------------------------------------------------------------------

locationHtml
    :: (Monad m, HasIdentifier c Checklist)
    => Maybe c -> Location -> HtmlT m ()
locationHtml mlist l = a_ [ href, title_ locName ] $ locSlug
  where
    href = indexPageHref (Just l) mlist (Nothing :: Maybe Task) False

    locSlug = case l of
        LocHelsinki  -> "Hel"
        LocTampere   -> "Tre"
        LocBerlin    -> "Ber"
        LocLondon    -> "Lon"
        LocStockholm -> "Sto"
        LocMunich    -> "Mun"
        LocOther     -> "Oth"
    locName = case l of
        LocHelsinki  -> "Helsnki"
        LocTampere   -> "Tampere"
        LocBerlin    -> "Berlin"
        LocLondon    -> "London"
        LocStockholm -> "Stockholm"
        LocMunich    -> "Munich"
        LocOther     -> "Other"

roleHtml
    :: (Monad m, HasIdentifier c Checklist)
    => Maybe c -> TaskRole -> HtmlT m ()
roleHtml mlist role = a_ [ href, title_ roleName ] $ toHtml $ roleName
  where
    roleName = role ^. re _TaskRole
    href = tasksPageHref (Just role) mlist

-- | Permamant status isn't shown, because it's common scenario: other contract
-- types stand up better.
contractTypeHtml :: Monad m => ContractType -> HtmlT m ()
contractTypeHtml ContractTypePermanent    = pure ()
contractTypeHtml ContractTypeExternal     = span_ [title_ "External"]      "Ext"
contractTypeHtml ContractTypeFixedTerm    = span_ [title_ "Fixed term"]    "Fix"
contractTypeHtml ContractTypePartTimer    = span_ [title_ "Part timer"]    "Part"
contractTypeHtml ContractTypeSummerWorker = span_ [title_ "Summer worker"] "Sum"

-- | TODO: better error
checklistNameHtml :: Monad m => World -> Maybe Location -> Identifier Checklist -> Bool -> HtmlT m ()
checklistNameHtml world mloc i notDone =
    a_ [ indexPageHref mloc (Just i) (Nothing :: Maybe Task) notDone ] $
        world ^. worldLists . at i . non (error "Inconsisten world") . nameHtml

-------------------------------------------------------------------------------
-- TodoCounter
-------------------------------------------------------------------------------

toTodoCounter :: World -> TaskRole -> Identifier Task -> TaskItem -> TodoCounter
toTodoCounter world tr tid td =
    case (has (worldTasks . ix tid . taskRole . only tr) world, td) of
        (True,  TaskItemDone) -> TodoCounter 1 1 1 1
        (True,  TaskItemTodo) -> TodoCounter 0 1 0 1
        (False, TaskItemDone) -> TodoCounter 0 0 1 1
        (False, TaskItemTodo) -> TodoCounter 0 0 0 1

data TodoCounter = TodoCounter !Int !Int !Int !Int
instance Semigroup TodoCounter where
    TodoCounter a b c d <> TodoCounter a' b' c' d' =
        TodoCounter (a + a') (b + b') (c + c') (d + d')
instance Monoid TodoCounter where
    mempty = TodoCounter 0 0 0 0
    mappend = (<>)

-------------------------------------------------------------------------------
-- Tasks
-------------------------------------------------------------------------------

taskCheckbox :: Monad m => World -> Employee -> Task -> HtmlT m ()
taskCheckbox world employee task = do
    checkbox_ checked
        [ id_ megaid
        , futuId_ "task-done-checkbox"
        , data_ "futu-employee" $ employee ^. identifierText
        , data_ "futu-task" $ task ^. identifierText
        ]
    label_ [ attrfor_ megaid ] $ task ^. nameHtml
  where
    checked = flip has world
        $ worldTaskItems
        . ix (employee ^. identifier)
        . ix (task ^. identifier)
        . _TaskItemDone

    megaid :: Text
    megaid =
        "task-checkbox-" <>
        employee ^. identifier . uuid . to UUID.toText <>
        "_" <>
        task ^. identifier . uuid . to UUID.toText

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

-- | If @show-all@ isn't specified, we assume it is.
--
-- /Note:/ Changing this to 'True' won't work, as the absence of flag == False.
defaultShowAll :: Bool
defaultShowAll = False
