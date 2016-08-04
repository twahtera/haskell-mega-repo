{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Theme.Types where

import Futurice.Prelude

import Codec.Picture.Types (PixelCMYK8 (..), PixelRGB8 (..))
import Futurice.Servant    (NamedSchema (..), ToSchema (..))
import Futurice.Colour
       (Colour (..), colourCMYK8, colourClay, colourName, colourPantoneName,
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

instance ToSchema IndexPage where
    declareNamedSchema _ = pure $ NamedSchema (Just "Indexpage") mempty

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
                h3_ "Design book"
                a_ [href_ $ "/images/Futurice_Guide_v4.pdf" ] $ "Design book"

colors :: Monad m => HtmlT m ()
colors = do
    row' $ do
        largemedium_ 6 $ colorBox Large FutuGreen
        largemedium_ 6 $ colorBox Large FutuBlack
    row' $ do
        largemedium_ 6 $ colorBox Medium FutuLightGreen
        largemedium_ 6 $ colorBox Medium FutuDarkGreen
    for_ [minBound .. maxBound ] $ \fam ->
        row' $ for_ [minBound .. maxBound ] $ \col ->
            largemedium_ 4 $ colorBox Small $ FutuAccent fam col

flipMaybe :: a -> Maybe b -> (b -> a) -> a
flipMaybe d m f = maybe d f m

data BoxSize = Large | Medium | Small

colorBox :: Monad m => BoxSize -> Colour -> HtmlT m ()
colorBox boxSize colour = div_ [style_ $ render css ] $ do
    flipMaybe (pure ()) (colourName colour) $ \name -> do
        b_ $ toHtml $ name
        br_ []
    b_ $ toHtml $ colourPantoneName colour
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
        Clay.height $ Clay.em $ case boxSize of
            Large  -> 16
            Medium -> 10
            Small  -> 8
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
