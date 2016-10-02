{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Markup (
    nonAuthorizedPage,
    indexPage,
    tasksPage,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Lens
       (Getter, filtered, has, ifoldMapOf, foldMapOf, non, only, re, to, _Wrapped)
import Data.List           (sortOn)
import Data.Maybe          (catMaybes)
import Data.Time           (addDays, diffDays)
import Servant.Utils.Links (URI (..), safeLink)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Types
import Futurice.Lucid.Foundation

import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified FUM

nonAuthorizedPage :: HtmlPage sym
nonAuthorizedPage = page_ "Non-authorized" pageParams $ do
    row_ $ large_ 12 $ header_ $ h1_ $ "Non-authorized"
    row_ $ large_ 12 $ p_ $
        "Ask IT-team to create you an account."

indexPage
    :: World       -- ^ the world
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Maybe Location
    -> Maybe Checklist
    -> Maybe Task
    -> HtmlPage "indexpage"
indexPage world today authUser@(_fu, viewerRole, _viewerLocation) mloc mlist mtask =
    let employees0 = sortOn (view employeeStartingDay) $ world ^.. worldEmployees . folded
        employees1 = maybe id (\l -> filter (has $ employeeLocation . only l)) mloc $ employees0
        employees2 = maybe id (\cl -> filter (has $ employeeChecklist . only (cl ^. identifier))) mlist $ employees1
        employees3 = maybe id (filter . taskPredicate) mtask employees2
        employees' = employees3

        taskPredicate :: Task -> Employee -> Bool
        taskPredicate task employee = flip has world $
            worldTaskItems . ix (employee ^. identifier) . ix (task ^. identifier)

    in page_ "Checklist" pageParams $ do
        navigation authUser

        -- Title
        header "Active employees"
            [ (^. re _Location) <$> mloc
            , (^. nameText ) <$> mlist
            ]

        -- List filtering controls
        row_ $ form_ [ action_ "/", method_ "get" ] $ do
            largemed_ 3 $ label_ $ do
                "Location"
                select_ [ name_ "location"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ [ minBound .. maxBound ] $ \loc ->
                        optionSelected_ (Just loc == mloc)
                            [ value_ $ loc ^. re _Location ]
                            $ toHtml $ locationToText loc
            largemed_ 3 $ label_ $ do
                "Checklist"
                select_ [ name_ "checklist"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldLists . folded) $ \cl ->
                        optionSelected_ (Just cl == mlist)
                            [ value_ $ cl ^. identifier . to identifierToText ]
                            $ cl ^. nameHtml
            largemed_ 5 $ label_ $ do
                "Task"
                select_ [ name_ "task" ] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldTasks . folded) $ \task ->
                        optionSelected_ (mtask ^? _Just . identifier == Just (task ^. identifier))
                            [ value_ $ task ^. identifier . to identifierToText ] $ do
                                task ^. nameHtml
                                " "
                                countEmployeesWithTask world task employees2
            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                button_ [ class_ "button" ] $ "Filter"

        -- The table
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ [title_ "Status"]                      "S"
                th_ [title_ "Location"]                    "Loc"
                th_ [title_ "Name" ]                       "Name"
                maybe
                    (th_ [title_ "Checklist"]                   "List")
                    (\task -> th_ [title_ "Selected task" ] $ task ^. nameHtml)
                    mtask
                th_ [title_ "Due date"]                    "Due date"
                th_ [title_ "Confirmed - contract signed"] "Confirmed"
                th_ [title_ "Days till start"]             "ETA"
                viewerItemsHeader viewerRole
                th_ [title_ "Task items todo/done"]        "Tasks"
            tbody_ $ for_ employees' $ \employee -> do
                let eid = employee ^. identifier
                let firstFutureDay = employees' ^? folded . employeeStartingDay . filtered (> today)
                let startingDay = employee ^. employeeStartingDay
                let etaClass day = case compare day today of
                        -- TODO: magic numbers
                        LT | day < addDays (- 30) today          -> "eta-far-past"
                           | otherwise                           -> "eta-past"
                        EQ                                       -> "eta-today"
                        GT | maybe False (day <=) firstFutureDay -> "eta-near-future"
                           | day > addDays 30 today              -> "eta-far-future"
                           | otherwise                           -> "eta-future"
                tr_ [ class_ $ etaClass $ employee ^. employeeStartingDay ] $ do
                    td_ $ contractTypeHtml $ employee ^. employeeContractType
                    td_ $ locationHtml mlist $ employee ^. employeeLocation
                    -- TODO: use safeLink
                    td_ $ a_ [ href_ $ "/employee/" <> employee ^. identifier . to identifierToText ] $ toHtml $
                        employee ^. employeeFirstName <> " " <> employee ^. employeeLastName
                    td_ $ maybe
                        (checklistNameHtml world mloc $ employee ^. employeeChecklist)
                        (taskCheckbox world employee)
                        mtask
                    td_ $ toHtml $ show startingDay
                    td_ $ bool (toHtmlRaw ("&#8868;" :: Text)) (pure ()) $ employee ^. employeeConfirmed
                    td_ $ toHtml $ show (diffDays startingDay today) <> " days"
                    case ifoldMapOf
                        (worldTaskItems . ix eid . ifolded)
                        (toTodoCounter world viewerRole)
                        world
                      of
                        TodoCounter a b i j -> do
                            td_ $ toHtml (show a) *> "/" *> toHtml (show b)
                            td_ $ toHtml (show i) *> "/" *> toHtml (show j)

tasksPage
    :: World                                 -- ^ the world
    -> (FUM.UserName, TaskRole, Location)    -- ^ logged in user
    -> Maybe TaskRole
    -> Maybe Checklist
    -> HtmlPage "tasks"
tasksPage world authUser@(_fu, _viewerRole, _viewerLocation) mrole mlist =
    let tasks0 = world ^.. worldTasks . folded
        tasks1 = maybe id (filter . rolePredicate) mrole tasks0
        tasks2 = maybe id (filter . checklistPredicate) mlist tasks1
        tasks' = tasks2

        rolePredicate :: TaskRole -> Task -> Bool
        rolePredicate role task = role == task ^. taskRole

        checklistPredicate :: Checklist -> Task -> Bool
        checklistPredicate cl task = flip has world $
            worldLists . ix (cl ^. identifier) . checklistTasks . ix (task ^. identifier)

    in page_ "Checklist - Tasks" pageParams $ do
        navigation authUser

        -- Title
        header "Tasks"
            [ (^. re _TaskRole) <$> mrole
            , (^. nameText ) <$> mlist
            ]

        -- List filtering controls
        row_ $ form_ [ action_ $ "/tasks", method_ "get" ] $ do
            largemed_ 3 $ label_ $ do
                "Role"
                select_ [ name_ "role"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ [ minBound .. maxBound ] $ \role ->
                        optionSelected_ (Just role == mrole)
                            [ value_ $ role ^. re _TaskRole ]
                            $ toHtml $ roleToText role
            largemed_ 8 $ label_ $ do
                "Checklist"
                select_ [ name_ "checklist"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldLists . folded) $ \cl ->
                        optionSelected_ (Just cl == mlist)
                            [ value_ $ cl ^. identifier . to identifierToText ]
                            $ cl ^. nameHtml
            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                button_ [ class_ "button" ] $ "Filter"

        -- The table
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ [ title_ "Task" ]                       "Task"
                th_ [ title_ "Role" ]                       "Role"
                th_ [ title_ "Active employees todo/done" ] "Employees"

            tbody_ $ for_ tasks' $ \task -> tr_ $ do
                let tid = task ^. identifier

                td_ $ a_ [ indexPageHref Nothing mlist (Just tid) ] $ task ^. nameHtml
                td_ $ roleHtml mlist (task ^. taskRole)
                td_ $ case foldMapOf (worldTaskItems' . ix tid . folded) countUsers world of
                    TodoCounter _ _ i j -> td_ $ toHtml (show i) *> "/" *> toHtml (show j)
 where
  countUsers TaskItemDone = TodoCounter 0 0 1 1
  countUsers TaskItemTodo = TodoCounter 0 0 0 1

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => AuthUser -> HtmlT m ()
navigation (fu, viewerRole, _viewerLocation) = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $ do
                "Checklist"
                sup_ "2"
            li_ $ a_ [ indexPageHref Nothing (Nothing :: Maybe Checklist) (Nothing :: Maybe Task) ] "Employees"
            li_ $ a_ [ href_ "#"] "Checklists"
            li_ $ a_ [ tasksPageHref Nothing (Nothing :: Maybe Checklist) ] "Tasks"
            li_ $ a_ [ href_ "#" ] "Reminder lists"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "dropdown menu" ] $
            li_ [ class_ "menu-text" ] $ do
                "Hello "
                toHtml $ fu ^. FUM.getUserName
                ", you are "
                roleHtml (Nothing :: Maybe Checklist) viewerRole

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

indexPageHref
    :: (HasIdentifier c Checklist, HasIdentifier t Task)
    => Maybe Location -> Maybe c -> Maybe t -> Attribute
indexPageHref mloc mlist mtask =
    href_ $ uriText $ safeLink checklistApi indexPageEndpoint mloc
        (mlist ^? _Just . identifier . uuid)
        (mtask ^? _Just . identifier . uuid)

tasksPageHref
    :: (HasIdentifier c Checklist)
    => Maybe TaskRole -> Maybe c -> Attribute
tasksPageHref mrole mlist =
    href_ $ uriText $ safeLink checklistApi tasksPageEndpoint mrole
        (mlist ^? _Just . identifier . uuid)

-------------------------------------------------------------------------------
-- Miscs
-------------------------------------------------------------------------------

countEmployeesWithTask :: Monad m => World -> Task -> [Employee] -> HtmlT m ()
countEmployeesWithTask world task = toHtml' . foldMap f
  where
    toHtml' (TodoCounter _ _ i j) =
      "(" *> toHtml (show i) *> "/" *> toHtml (show j) *> ")"

    f employee = case world ^? worldTaskItems . ix (employee ^. identifier) . ix (task ^. identifier) of
        Nothing           -> TodoCounter 0 0 0 0
        Just TaskItemTodo -> TodoCounter 0 0 0 1
        Just TaskItemDone -> TodoCounter 0 0 1 1

taskCheckbox :: Monad m => World -> Employee -> Task -> HtmlT m ()
taskCheckbox world employee task = do
    checkbox_ checked [ id_ megaid ]
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

locationHtml
    :: (Monad m, HasIdentifier c Checklist)
    => Maybe c -> Location -> HtmlT m ()
locationHtml mlist l = a_ [ href, title_ locName ] $ locSlug
  where
    href = indexPageHref (Just l) mlist (Nothing :: Maybe Task)

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
checklistNameHtml :: Monad m => World -> Maybe Location -> Identifier Checklist -> HtmlT m ()
checklistNameHtml world mloc i =
    a_ [ indexPageHref mloc (Just i) (Nothing :: Maybe Task) ] $
        world ^. worldLists . at i . non (error "Inconsisten world") . nameHtml

uriText :: URI -> Text
uriText (URI _ _ path query _) = ("/" <> path <> query) ^. packed

viewerItemsHeader :: Monad m => TaskRole -> HtmlT m ()
viewerItemsHeader TaskRoleIT         = th_ [title_ "IT tasks todo/done"]          "IT items"
viewerItemsHeader TaskRoleHR         = th_ [title_ "HR tasks todo/done"]          "HR items"
viewerItemsHeader TaskRoleSupervisor = th_ [title_ "Supervisor tasks todo/done"]  "Supervisor items"

toTodoCounter :: World -> TaskRole -> Identifier Task -> TaskItemDone -> TodoCounter
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
