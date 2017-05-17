{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FUM.Markup (
    -- * Structure
    fumPage_,
    fumHeader_,
    subheader_,
    -- * Form
    futuId_,
    futuLinkButton_,
    -- * Re-export
    module Futurice.App.FUM.Markup.Href,
    module Futurice.Lucid.Foundation,
    ) where

import Futurice.App.FUM.Clay     (pageParams)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Lucid.Base                (Attribute (..))
import Prelude ()

import Futurice.App.FUM.Markup.Href hiding (linkToText)

import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

-- | TODO
type AuthUser = ()

fumPage_ :: Text -> AuthUser -> Html () -> HtmlPage sym
fumPage_ title authUser body =
    page_ (title <> " - FUM⁶" ) pageParams $ do
        navigation authUser
        div_ [ futuId_ "error-callout", class_ "callout alert", style_ "display: none" ] $ do
            div_ [ futuId_ "error-callout-content" ] $ pure ()
            button_ [ class_ "button" ] "Close"
        body

-- http://foundation.zurb.com/sites/docs/top-bar.html
navigation :: Monad m => AuthUser -> HtmlT m ()
navigation () = do
    div_ [ class_ "top-bar" ] $ do
        div_ [ class_ "top-bar-left" ] $ ul_ [ class_ "dropdown menu" ] $ do
            li_ [ class_ "menu-text"] $
                a_ [ indexPageHref_ ] $ do
                    "FUM"
                    sup_ "6"
            li_ $ a_ [ id_ "futu-reload-indicator", href_ "#", style_ "display: none", title_ "You made changes, refresh page to show" ]  "1"
            li_ $ a_ [ href_ "#" ] "Employees"
            li_ $ a_ [ href_ "#" ] "Groups"
            li_ $ a_ [ href_ "#" ] "Mailboxes"
            li_ $ a_ [ href_ "#" ] "Customers"
        div_ [ class_ "top-bar-right" ] $ ul_ [ class_ "dropdown menu" ] $
            li_ [ class_ "menu-text" ] $ do
                "Hello "
                "guest"  -- TODO: make SSO self-work

fumHeader_
    :: Monad m
    => Text          -- ^ default title
    -> [Maybe Text]  -- ^ title parts
    -> HtmlT m ()
fumHeader_ title titleParts' = row_ $ large_ 12 $ header_ $ h1_ $ toHtml $
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
-- Forms
-------------------------------------------------------------------------------

futuId_ :: Text -> Attribute
futuId_ = data_ "futu-id"

futuLinkButton_ :: Monad m => Attribute -> HtmlT m () -> HtmlT m ()
futuLinkButton_ (Attribute _ href) = button_
    [ class_ "button"
    , data_ "futu-link-button" href
    , disabled_ "disabled"
    ]

-- futuForm_ :: Monad m => Text -> [Attribute] -> HtmlT m () -> HtmlT m ()
-- futuForm_ i attrs = row_ . large_ 12 . form_ (futuId_ i : attrs)
