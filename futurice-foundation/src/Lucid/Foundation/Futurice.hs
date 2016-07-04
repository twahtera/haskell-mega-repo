{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Lucid.Foundation.Futurice (
    -- * Embedded style
    embeddedFoundationStyle_,
    -- * Grid
    row_,
    large_,
    -- * Page
    page_,
    -- * Re-exports
    module Lucid.Foundation,
    ) where

import Data.FileEmbed   (embedStringFile)
import Data.String      (fromString)
import Data.Text        (Text)
import Lucid
import Lucid.Foundation hiding (large_, row_)

import qualified Data.Text as T

embeddedFoundationStyle_ :: Monad m => HtmlT m ()
embeddedFoundationStyle_ =
    style_ [type_ "text/css"] ($(embedStringFile "foundation.min.css") :: String)

row_ :: Monad m => HtmlT m () -> HtmlT m ()
row_ = div_ [class_ "row"]

large_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
large_ n = div_ [class_ $ fromString $ "columns large-" ++ show n ]

-- | Page template.
page_ :: Monad m => Text -> HtmlT m () -> HtmlT m ()
page_ t b = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml t
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "x-ua-compatible", content_"ie=edge"]
        embeddedFoundationStyle_
        -- TODO: rework, use @clay@
        style_ $ T.unlines
            [ ".emphasize td { font-weight: bold; background: #eee }"
            , ".emphasize2 td { font-style: italic; background: #efe; }"
            , "h1, h2, h3, h4, li, td, div, span, b { font-family: \"Lucida Grande\", Helvetica, Arial, sans-serif; }"
            , "* { font-size: 11pt; }"
            , "h1 { font-size: 20pt; font-weight: bold; }"
            , "h2 { font-size: 15pt; font-weight: bold; }"
            , "h3 { font-size: 13pt; font-weight: bold; }"
            ]
    body_ b
