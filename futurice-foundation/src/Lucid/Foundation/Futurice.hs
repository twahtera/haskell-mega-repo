{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}
module Lucid.Foundation.Futurice (
    -- * Embedded style
    embeddedFoundationStyle_,
    -- * Grid
    row_,
    large_,
    largemed_,
    -- * Form
    optionSelected_,
    -- * Page
    page_,
    PageParams,
    pageCss,
    pageJs,
    defPageParams,
    ) where

import Prelude ()
import Futurice.Prelude

import Data.FileEmbed (embedStringFile)
import Clay (Css, render)
import Lucid hiding (for_)

import qualified Data.Text as T

embeddedFoundationStyle_ :: Monad m => HtmlT m ()
embeddedFoundationStyle_ =
    style_ [type_ "text/css"] ($(embedStringFile "foundation.min.css") :: String)

embeddedLodash_ :: Monad m => HtmlT m ()
embeddedLodash_ =
    script_ ($(embedStringFile "lodash.js") :: Text)

-------------------------------------------------------------------------------
-- Grid
-------------------------------------------------------------------------------

row_ :: Monad m => HtmlT m () -> HtmlT m ()
row_ = div_ [class_ "row"]

large_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
large_ n = div_ [class_ $ fromString $ "columns large-" ++ show n ]

largemed_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
largemed_ n = div_
    [ class_ $ "columns large-" <> textShow n <> " medium-" <> textShow n ]

-------------------------------------------------------------------------------
-- Form
-------------------------------------------------------------------------------

optionSelected_ :: Term arg result => Bool -> arg -> result
optionSelected_ True  = termWith "option" [ selected_ "selected "]
optionSelected_ False = term "option"

-------------------------------------------------------------------------------
-- Page
-------------------------------------------------------------------------------

data PageParams = PageParams
    { _pageCss :: [Css]
    , _pageJs  :: [Text]
    }

defPageParams :: PageParams
defPageParams = PageParams [] []

makeLenses ''PageParams

-- TODO: create submodule, move there

-- | Similar to 'Term' from @lucid@.
class Page arg result | result -> arg where
    -- | Page template.
    page_ :: Text -> arg -> result

instance Monad m => Page (HtmlT m ()) (HtmlT m ()) where
    page_ t = pageImpl t defPageParams

instance (Monad m, a ~ HtmlT m (), b ~ HtmlT m ()) => Page PageParams (a -> b) where
    page_ = pageImpl

pageImpl :: Monad m => Text -> PageParams -> HtmlT m () -> HtmlT m ()
pageImpl t p b = doctypehtml_ $ do
    head_ $ do
        title_ $ toHtml t
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
        meta_ [httpEquiv_ "x-ua-compatible", content_"ie=edge"]
        embeddedFoundationStyle_
        embeddedLodash_
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
        -- additional styles
        for_ (p ^. pageCss) $ style_ . view strict . render
        -- additional js
        for_ (p ^. pageJs) $ script_ []
    body_ b
