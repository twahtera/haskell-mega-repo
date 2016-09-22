{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Control.Lens
       (at, filtered, has, ifoldMapOf, ix, non, only, to, (^?))
import Data.List                 (sortOn)
import Data.Time                 (addDays, diffDays)
import Futurice.Servant
import Lucid                     hiding (for_)
import Lucid.Foundation.Futurice
import Servant
import Test.QuickCheck           (arbitrary, generate, resize)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Types

import qualified FUM

-- TODO: make to .Types.Ctx
type Ctx = World

server :: Ctx -> Server ChecklistAPI
server ctx = liftIO . indexPage ctx

currentDay :: IO Day
currentDay = utctDay <$> currentTime

-- TODO: use safe link
locHtml :: Monad m => Location -> HtmlT m ()
locHtml l = a_ [ href_ ("/location/" <> locSlug), title_ locName ] $ toHtml locSlug
  where
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

-- | Permamant status isn't shown, because it's common scenario: other contract
-- types stand up better.
contractTypeHtml :: Monad m => ContractType -> HtmlT m ()
contractTypeHtml ContractTypePermanent    = pure ()
contractTypeHtml ContractTypeExternal     = span_ [title_ "External"]      "Ext"
contractTypeHtml ContractTypeFixedTerm    = span_ [title_ "Fixed term"]    "Fix"
contractTypeHtml ContractTypePartTimer    = span_ [title_ "Part timer"]    "Part"
contractTypeHtml ContractTypeSummerWorker = span_ [title_ "Summer worker"] "Sum"

-- | TODO: better error
checklistNameHtml :: Monad m => World -> Identifier Checklist -> HtmlT m ()
checklistNameHtml world i =
    a_ [ href_ $ "/checklist/" <> i ^. to identifierToText] $ toHtml $
        world ^. worldLists . at i . non (error "Inconsisten world") . checklistName . to show

-- | TODO: context
viewerRole :: TaskRole
viewerRole = TaskRoleIT

indexPage :: Ctx -> Maybe FUM.UserName -> IO (Page "indexpage")
indexPage world fu = do
    today <- currentDay
    let employees = sortOn (view employeeStartingDay) $ world ^.. worldEmployees . folded
    pure $ Page $ page_ "Checklist" pageParams $ do
        -- http://foundation.zurb.com/sites/docs/top-bar.html
        div_ [ class_ "top-bar" ] $ do
            div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
                li_ [ class_ "menu-text"] $ do
                    "Checklist"
                    sup_ "2"
                li_ $ a_ [ href_ "#" ] "Employees"
                li_ $ a_ [ href_ "#" ] "Checklists"
                li_ $ a_ [ href_ "#" ] "Tasks"
                li_ $ a_ [ href_ "#" ] "Reminder lists"
            div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "dropdown menu" ] $
                li_ [ class_ "menu-text" ] $ do
                    "Hello "
                    maybe (em_ "Guest") (toHtml . view FUM.getUserName) fu

        row_ $ large_ 12 $ header_ $ h1_ $ "Active employees"

        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ [title_ "Status"]                      "S"
                th_ [title_ "Location"]                    "Loc"
                th_ [title_ "Name" ]                       "Name"
                th_ [title_ "Checklist"]                   "List"
                th_ [title_ "Due date"]                    "Due date"
                th_ [title_ "Confirmed - contract signed"] "Confirmed"
                th_ [title_ "Days till start"]             "ETA"
                th_ [title_ "IT task todo/done"]           "IT items" -- Title based on viewerRole
                th_ [title_ "Task items todo/done"]        "Items"
            tbody_ $ for_ employees $ \employee -> do
                let eid = employee ^. identifier
                let firstFutureDay = employees ^? folded . employeeStartingDay . filtered (> today)
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
                    td_ $ locHtml $ employee ^. employeeLocation
                    -- TODO: use safeLink
                    td_ $ a_ [ href_ $ "/employee/" <> employee ^. identifier . to identifierToText ] $ toHtml $
                        employee ^. employeeFirstName <> " " <> employee ^. employeeLastName
                    td_ $ checklistNameHtml world (employee ^. employeeChecklist)
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

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Checklist API"
    "Super TODO"
    (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    (pure ()) (const 8000) -- getConfig cfgPort
    checklistApi server futuriceNoMiddleware
    $ \_ _cache -> -- do
        generate (resize 200 arbitrary)
