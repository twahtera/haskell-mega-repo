{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Index (indexPage) where

import Prelude ()
import Futurice.Prelude
import Control.Lens
       (filtered, has, hasn't, ifoldMapOf, only, re, to, united)
import Data.Time                 (addDays, diffDays)
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

indexPage
    :: World       -- ^ the world
    -> Day         -- ^ today
    -> AuthUser    -- ^ logged in user
    -> Maybe Location
    -> Maybe Checklist
    -> Maybe Task
    -> Bool
    -> HtmlPage "indexpage"
indexPage world today authUser@(_fu, viewerRole) mloc mlist mtask showAll =
    let employees0 = sortOn (view employeeStartingDay) $ world ^.. worldEmployees . folded
        employees1 = maybe id (\l -> filter (has $ employeeLocation . only l)) mloc $ employees0
        employees2 = maybe id (\cl -> filter (has $ employeeChecklist . only (cl ^. identifier))) mlist $ employees1
        employees3 = maybe id (filter . taskPredicate) mtask employees2
        employees' = employees3

        taskPredicate :: Task -> Employee -> Bool
        taskPredicate task employee = flip has world $ worldTaskItems
            . ix (employee ^. identifier)
            . ix (task ^. identifier)
            . taskItemPredicate

        -- Single item traversal which we use as a filter in taskPredicate
        taskItemPredicate :: Traversal' AnnTaskItem ()
        taskItemPredicate | showAll   = united
                          | otherwise = _AnnTaskItemTodo . united

    in checklistPage_ "Employees" authUser $ do
        -- Title
        header "Active employees"
            [ (^. re _Location) <$> mloc
            , (^. nameText) <$> mlist
            , (^. nameText) <$> mtask
            ]

        -- List filtering controls
        row_ $ form_ [ futuId_ "selector", action_ "/", method_ "get" ] $ do
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
            largemed_ 4 $ label_ $ do
                "Task"
                select_ [ name_ "task" ] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldTasksSortedByName . folded) $ \task ->
                        optionSelected_ (mtask ^? _Just . identifier == Just (task ^. identifier))
                            [ value_ $ task ^. identifier . to identifierToText ] $ do
                                task ^. nameHtml
                                " "
                                countEmployeesWithTask world task employees2

            largemed_ 1 $ label_ $ do
                "all"
                div_ $ checkbox_ showAll [ name_ "show-all", value_ "true", title_ "Show also people with task already done" ]

            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                div_ $ button_ [ class_ "button" ] $ "Filter"

        for_ mtask $ \task -> when (hasn't (taskInfo . _Empty) task) $ do
            row_ $ large_ 12 $ do
                b_ "Task info: "
                toHtml $ task ^. taskInfo
                hr_ []

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
                -- for_ mtask $ \_task -> th_ [ title_ "Additional info for task + employee" ] "Task info"
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
                    td_ $ employeeLink employee
                    td_ $ maybe
                        (checklistNameHtml world mloc (employee ^. employeeChecklist) showAll)
                        (taskCheckbox world employee)
                        mtask
                    --for_ mtask $ \task -> td_ $
                    --    input_ [ type_ "text" ] -- TODO: taskInfoInput world employee
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
        Nothing                    -> TodoCounter 0 0 0 0
        Just AnnTaskItemTodo       -> TodoCounter 0 0 0 1
        Just (AnnTaskItemDone _ _) -> TodoCounter 0 0 1 1

viewerItemsHeader :: Monad m => TaskRole -> HtmlT m ()
viewerItemsHeader TaskRoleIT         = th_ [title_ "IT tasks todo/done"]          "IT items"
viewerItemsHeader TaskRoleHR         = th_ [title_ "HR tasks todo/done"]          "HR items"
viewerItemsHeader TaskRoleSupervisor = th_ [title_ "Supervisor tasks todo/done"]  "Supervisor items"
