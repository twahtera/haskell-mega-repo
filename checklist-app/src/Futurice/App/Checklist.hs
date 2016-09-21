{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Control.Lens              (to, at, non)
import Data.List                 (sortOn)
import Data.Ord                  (Down (..))
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
    pure $ Page $ page_ "Checklist" pageParams $ table_ $ do
        thead_ $ tr_ $ do
            th_ [title_ "Status"]                      "S"
            th_ [title_ "Location"]                    "Loc"
            th_ [title_ "Name" ]                       "Name"
            th_ [title_ "Checklist"]                   "List"
            th_ [title_ "Starting day"]                "Starts at"
            th_ [title_ "Confirmed - contract signed"] "Confirmed"
            th_ [title_ "Days till start"]             "ETA"
            th_ "Group items?"
            th_ [title_ "Task items todo/done"]        "Items"
        tbody_ $ for_ (sortOn (Down . view userStartingDay) $ toList users) $ \user -> do
            let eta = toModifiedJulianDay today - toModifiedJulianDay (user ^. userStartingDay)
            tr_ [class_ $ etaClass eta] $ do
                td_ $ contractTypeHtml $ user ^. userContractType
                td_ $ locHtml $ user ^. userLocation
                -- TODO: use safeLink
                td_ $ a_ [ href_ $ "/user/" <> user ^. identifier ^. to identifierToText ] $ toHtml $
                    user ^. userFirstName <> " " <> user ^. userLastName
                td_ $ checklistNameHtml world (user ^. userChecklist)
                td_ $ toHtml $ show $ user ^. userStartingDay
                td_ $ toHtml $ show $ user ^. userConfirmed
                td_ $ toHtml $ show eta <> " days"
                td_ "TODO"
                td_ "TODO"
  where
    etaClass eta = case compare 0 eta of
        EQ -> "eta-today"
        LT -> "eta-past"
        GT -> "eta-future"

defaultMain :: IO ()
defaultMain = futuriceServerMain
    "Checklist API"
    "Super TODO"
    (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    (pure ()) (const 8000) -- getConfig cfgPort
    checklistApi server futuriceNoMiddleware
    $ \_ _cache -> -- do
        generate (resize 200 arbitrary)
