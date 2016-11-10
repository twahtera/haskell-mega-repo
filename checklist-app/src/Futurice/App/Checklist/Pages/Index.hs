{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Index (indexPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (filtered, has, ifoldMapOf, only, re, to)
import Data.Time                 (addDays, diffDays)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.UUID as UUID

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
                    td_ $ bool (pure ()) (toHtmlRaw ("&#8868;" :: Text)) $ employee ^. employeeConfirmed
                    td_ $ toHtml $ show (diffDays startingDay today) <> " days"
                    case ifoldMapOf
                        (worldTaskItems . ix eid . ifolded)
                        (toTodoCounter world viewerRole)
                        world
                      of
                        TodoCounter a b i j -> do
                            td_ $ toHtml (show a) *> "/" *> toHtml (show b)
                            td_ $ toHtml (show i) *> "/" *> toHtml (show j)

-------------------------------------------------------------------------------
-- Utilities
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

viewerItemsHeader :: Monad m => TaskRole -> HtmlT m ()
viewerItemsHeader TaskRoleIT         = th_ [title_ "IT tasks todo/done"]          "IT items"
viewerItemsHeader TaskRoleHR         = th_ [title_ "HR tasks todo/done"]          "HR items"
viewerItemsHeader TaskRoleSupervisor = th_ [title_ "Supervisor tasks todo/done"]  "Supervisor items"
