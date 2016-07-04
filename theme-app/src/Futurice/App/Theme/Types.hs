{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Theme.Types where

import Futurice.Prelude

import Codec.Picture.Types (PixelCMYK8 (..), PixelRGB8 (..))
import Futurice.Colour     (Colour (..), colourCMYK8, colourClay, colourName,
                            colourRGB8)
import Numeric             (showHex)

import Lucid                     hiding (for_)
import Lucid.Foundation.Futurice

import qualified Clay
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Indexpage
-------------------------------------------------------------------------------

data IndexPage = IndexPage

instance ToHtml IndexPage where
    toHtmlRaw = toHtml
    toHtml _ = page_ "Futurice colors and logos" $ do
        row_ $ large_ 12 $ h1_ "Futurice colors and logos"
        row_ $ do
            large_ 9 $ do
                h3_ "Colors"
                colors
            large_ 3 $ do
                h3_ "Download logos"
                ul_ $ for_ logos $ \(name, url) ->
                    li_ $ a_ [href_ $ "/images/" <> url ] $ toHtml name

colors :: Monad m => HtmlT m ()
colors = do
    row' $ do
        largemedium_ 6 $ colorBox True FutuGreen
        largemedium_ 6 $ colorBox True FutuBlack
    for_ [minBound .. maxBound ] $ \fam ->
        row' $ for_ [minBound .. maxBound ] $ \col ->
            largemedium_ 4 $ colorBox False $ FutuAccent fam col

colorBox :: Monad m => Bool -> Colour -> HtmlT m ()
colorBox big colour = div_ [style_ $ render css ] $ do
    b_ $ toHtml $ colourName colour
    br_ []
    toHtml $ rgbDesc
    br_ []
    toHtml $ cmykDesc
    br_ []
    toHtml $ hexDesc
  where
    -- Howto make inline styles with clay?
    render x = T.drop 1 $ T.dropEnd 1 $ Clay.renderWith Clay.compact [] x ^. strict
    css = do
        Clay.sym Clay.padding em1
        Clay.sym Clay.margin emhalf
        Clay.height $ Clay.em $ if big then 16 else 8
        Clay.backgroundColor $ bgColor
        Clay.fontColor $ fgColor

    emhalf = Clay.em 0.5
    em1 = Clay.em 1

    bgColor = colourClay colour
    fgColor = case Clay.toHsla bgColor of
        Clay.Hsla _ _ l _ | l < 0.5  -> Clay.white
        _                           -> Clay.black
    rgbDesc :: Text
    rgbDesc = case colourRGB8 colour of
        PixelRGB8 r g b -> mconcat
            [ "R: ", show r ^. packed, ", "
            , "G: ", show g ^. packed, ", "
            , "B: ", show b ^. packed
            ]

    cmykDesc :: Text
    cmykDesc = case colourCMYK8 colour of
        PixelCMYK8 c m y k -> mconcat
            [ "C: ", show c ^. packed, ", "
            , "M: ", show m ^. packed, ", "
            , "Y: ", show y ^. packed, ", "
            , "K: ", show k ^. packed
            ]

    hexDesc :: Text
    hexDesc = T.toUpper $ case colourRGB8 colour of
        PixelRGB8 r g b ->
            let str = showHex (toInteger r * 256 * 256 + toInteger g * 256 + toInteger b) ""
            in "HEX: #" <> (replicate (6 - length str) '0' ++ str) ^. packed

logos :: [(Text, Text)]
logos =
    [ ("PNG, Green, 960x250", "futurice.png")
    , ("PNG, Green, 3000x683", "futurice-300dpi.png")
    , ("PNG, Black, 960x250", "futurice-black.png")
    , ("PNG, White, 960x250", "futurice-white.png")
    , ("PNG, Green, 250x250", "futurice-square.png")
    , ("PNG, Green Favicon, 129x129", "futurice-favicon.png")
    , ("SVG, Green", "futurice.svg")
    , ("EPS, Green", "futurice.eps")
    ]

-------------------------------------------------------------------------------
-- Html helpers
-------------------------------------------------------------------------------

row' :: Monad m => HtmlT m () -> HtmlT m ()
row' = div_ [class_ "row large-collapse medium-collapse"]

largemedium_ :: Monad m => Int -> HtmlT m () -> HtmlT m ()
largemedium_ n = div_ [class_ $ fromString $ "columns large-" ++ show n ++ " medium-" ++ show n ]
