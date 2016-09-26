{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Futurice.App.Checklist (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Control.Lens
       (filtered, has, ifoldMapOf, non, only, re, to, (^?))
import Data.List                 (sortOn)
import Data.Maybe                (catMaybes)
import Data.Time                 (addDays, diffDays)
import Futurice.Servant
import Lucid                     hiding (for_)
import Lucid.Foundation.Futurice
import Servant
import Test.QuickCheck           (arbitrary, generate, resize)

import Futurice.App.Checklist.API
import Futurice.App.Checklist.Clay
import Futurice.App.Checklist.Config
import Futurice.App.Checklist.Types

import qualified Data.Text as T
import qualified FUM

-- TODO: make to .Types.Ctx
type Ctx = World

server :: Ctx -> Server ChecklistAPI
server ctx = indexPage ctx

indexPageHref
    :: HasIdentifier c Checklist
    => Maybe Location
    -> Maybe c
    -> Maybe (Identifier Task)
    -> Attribute
indexPageHref mloc mlist mtask =
    href_ $ uriText $ safeLink checklistApi indexPageEndpoint
        mloc (mlist ^? _Just . identifier . uuid) (view uuid <$> mtask)

locHtml
    :: (Monad m, HasIdentifier c Checklist)
    => Maybe c -> Location -> HtmlT m ()
locHtml mlist l = a_ [ href, title_ locName ] $ locSlug
  where
    href = indexPageHref (Just l) mlist Nothing

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
checklistNameHtml :: Monad m => World -> Maybe Location -> Identifier Checklist -> HtmlT m ()
checklistNameHtml world mloc i =
    a_ [ indexPageHref mloc (Just i) Nothing ] $ toHtml $
        world ^. worldLists . at i . non (error "Inconsisten world") . checklistName . to show

uriText :: URI -> Text
uriText (URI _ _ path query _) = (path <> query) ^. packed

indexPage
    :: MonadIO m
    => Ctx
    -> Maybe FUM.UserName
    -> Maybe Location
    -> Maybe UUID
    -> Maybe UUID
    -> m (Page "indexpage")
indexPage world fu loc cid _tid = case userInfo of
    Nothing        -> pure nonAuthorizedPage
    Just userInfo' -> liftIO $ indexPage' world userInfo' loc checklist
  where
    userInfo :: Maybe (FUM.UserName, TaskRole, Location)
    userInfo = world ^? worldUsers . ix fu . _Just

    checklist = do
        cid' <- cid
        world ^? worldLists . ix (Identifier cid')

nonAuthorizedPage :: Page sym
nonAuthorizedPage = Page $ page_ "Non-authorized" pageParams $ do
    row_ $ large_ 12 $ header_ $ h1_ $ "Non-authorized"
    row_ $ large_ 12 $ p_ $
        "Ask IT-team to create you an account."

nameToText :: Name a -> Text
nameToText (Name n) = n

indexPage'
    :: Ctx
    -> (FUM.UserName, TaskRole, Location)
    -> Maybe Location
    -> Maybe Checklist
    -> IO (Page "indexpage")
indexPage' world (fu, viewerRole, _viewerLocation) mloc mlist = do
    today <- currentDay
    let employees0  = sortOn (view employeeStartingDay) $ world ^.. worldEmployees . folded
        employees1 = maybe id (\l -> filter (has $ employeeLocation . only l)) mloc $ employees0
        employees2 = maybe id (\cl -> filter (has $ employeeChecklist . only (cl ^. identifier))) mlist $ employees1
        employees' = employees2
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
                    toHtml $ fu ^. FUM.getUserName
                    ", you are "
                    toHtml (showRole viewerRole)

        -- Title
        let titleParts = catMaybes
                [ (^. re _Location) <$> mloc
                , (^. checklistName . to nameToText ) <$> mlist
                ]
        row_ $ large_ 12 $ header_ $ h1_ $ toHtml $ if null titleParts
            then "Active employees"
            else T.intercalate " - " titleParts

        -- List filtering controls
        row_ $ form_ [ action_ "/", method_ "get" ]$ do
            largemed_ 3 $ label_ $ do
                "Location"
                select_ [ name_ "location"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ [ minBound .. maxBound ] $ \loc ->
                        optionSelected_ (Just loc == mloc) [ value_ $ loc ^. re _Location ] $ toHtml $ locationToText loc
            largemed_ 3 $ label_ $ do
                "Checklist"
                select_ [ name_ "checklist"] $ do
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldLists . folded) $ \cl ->
                        optionSelected_ (Just cl == mlist) [ value_ $ cl ^. identifier . to identifierToText ]
                            $ toHtml $ show $ cl ^. checklistName
            largemed_ 5 $ label_ $ do
                "Task"
                select_ $ do
                    -- TODO: select chosen
                    option_ [ value_ "" ] $ "Show all"
                    for_ (world ^.. worldTasks . folded) $ \task ->
                        option_ [ value_ $ task ^. identifier . to identifierToText ]
                            $ toHtml $ show $ task ^. taskName
            largemed_ 1 $ label_ $ do
                toHtmlRaw ("&nbsp;" :: Text)
                button_ [ class_ "button" ] $ "Filter"

        -- The table
        row_ $ large_ 12 $ table_ $ do
            thead_ $ tr_ $ do
                th_ [title_ "Status"]                      "S"
                th_ [title_ "Location"]                    "Loc"
                th_ [title_ "Name" ]                       "Name"
                th_ [title_ "Checklist"]                   "List"
                th_ [title_ "Due date"]                    "Due date"
                th_ [title_ "Confirmed - contract signed"] "Confirmed"
                th_ [title_ "Days till start"]             "ETA"
                viewerItemsHeader viewerRole
                th_ [title_ "Task items todo/done"]        "Items"
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
                    td_ $ locHtml mlist $ employee ^. employeeLocation
                    -- TODO: use safeLink
                    td_ $ a_ [ href_ $ "/employee/" <> employee ^. identifier . to identifierToText ] $ toHtml $
                        employee ^. employeeFirstName <> " " <> employee ^. employeeLastName
                    td_ $ checklistNameHtml world mloc (employee ^. employeeChecklist)
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

viewerItemsHeader :: Monad m => TaskRole -> HtmlT m ()
viewerItemsHeader TaskRoleIT         = th_ [title_ "IT tasks todo/done"]          "IT items"
viewerItemsHeader TaskRoleHR         = th_ [title_ "HR tasks todo/done"]          "HR items"
viewerItemsHeader TaskRoleSupervisor = th_ [title_ "Supervisor tasks todo/done"]  "Supervisor items"

showRole :: TaskRole -> Text
showRole TaskRoleIT         = "IT"
showRole TaskRoleHR         = "HR"
showRole TaskRoleSupervisor = "supervisor"

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
    getConfig cfgPort
    checklistApi server futuriceNoMiddleware
    $ \cfg _cache -> do
        world0 <- generate (resize 200 arbitrary)
        let world1 = if cfgMockAuth cfg
            then world0 & worldUsers .~ const (Just mockCredentials)
            else world0
        pure world1
  where
    mockCredentials = (FUM.UserName "phadej", TaskRoleIT, LocHelsinki)
