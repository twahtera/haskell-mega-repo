{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Control.Lens              (to, at, non, folded, foldMapOf)
import Data.List                 (sortOn)
import Futurice.Servant
import Lucid                     hiding (for_)
import Lucid.Foundation.Futurice
import Servant
import Test.QuickCheck           (arbitrary, generate, resize)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Types

-- TODO: make to .Types.Ctx
type Ctx = World

server :: Ctx -> Server ChecklistAPI
server ctx = liftIO (indexPage ctx)

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

indexPage :: Ctx -> IO (Page "indexpage")
indexPage world = do
    today <- currentDay
    let users = world ^. worldUsers
    pure $ Page $ page_ "Checklist" pageParams $ do
        -- http://foundation.zurb.com/sites/docs/top-bar.html
        div_ [ class_ "top-bar" ] $ do
            div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
                li_ [ class_ "menu-text"] $ do
                    "Checklist"
                    sup_ "2"
                li_ $ a_ [ href_ "#" ] "Users"
                li_ $ a_ [ href_ "#" ] "Checklists"
                li_ $ a_ [ href_ "#" ] "Tasks"
                li_ $ a_ [ href_ "#" ] "Reminder lists"

        row_ $ large_ 12 $ header_ $ h1_ $ "Active users"

        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ [title_ "Status"]                      "S"
                th_ [title_ "Location"]                    "Loc"
                th_ [title_ "Name" ]                       "Name"
                th_ [title_ "Checklist"]                   "List"
                th_ [title_ "Due date"]                    "Due date"
                th_ [title_ "Confirmed - contract signed"] "Confirmed"
                th_ [title_ "Days till start"]             "ETA"
                th_ "Group items?"
                th_ [title_ "Task items todo/done"]        "Items"
            tbody_ $ for_ (sortOn (view userStartingDay) $ toList users) $ \user -> do
                let eta =  toModifiedJulianDay (user ^. userStartingDay) - toModifiedJulianDay today
                tr_ [class_ $ etaClass eta] $ do
                    td_ $ contractTypeHtml $ user ^. userContractType
                    td_ $ locHtml $ user ^. userLocation
                    -- TODO: use safeLink
                    td_ $ a_ [ href_ $ "/user/" <> user ^. identifier ^. to identifierToText ] $ toHtml $
                        user ^. userFirstName <> " " <> user ^. userLastName
                    td_ $ checklistNameHtml world (user ^. userChecklist)
                    td_ $ toHtml $ show $ user ^. userStartingDay
                    td_ $ bool (toHtmlRaw ("&#8868;" :: Text)) (pure ()) $ user ^. userConfirmed
                    td_ $ toHtml $ show eta <> " days"
                    td_ "TODO"
                    td_ $ toHtml $ foldMapOf
                        (worldTaskItemsByUser . at (user ^. identifier) . non mempty . folded)
                        taskItemToTodoCounter
                        world
  where
    etaClass eta = case compare eta 0 of
        EQ -> "eta-today"
        LT -> "eta-past"
        GT -> "eta-future"

taskItemToTodoCounter :: TaskItem -> TodoCounter
taskItemToTodoCounter ti
    | ti ^. taskItemDone = TodoCounter 1 1
    | otherwise          = TodoCounter 0 1

data TodoCounter = TodoCounter !Int !Int
instance Semigroup TodoCounter where
    TodoCounter a b <> TodoCounter a' b' = TodoCounter (a + a') (b + b')
instance Monoid TodoCounter where
    mempty = TodoCounter 0 0
    mappend = (<>)
instance ToHtml TodoCounter where
    toHtmlRaw = toHtml
    toHtml (TodoCounter a b) = do
        toHtml (show a)
        "/"
        toHtml (show b)

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
