module Lucid.Foundation.Futurice.JavaScript (
    JS,
    getJS,
    makeJS,
    unsafeMakeJS,
    ) where

import Futurice.Prelude
import Lucid            (ToHtml (..), script_)
import Prelude ()

import qualified Language.JavaScript.Parser as JS

newtype JS = JS Text

getJS :: JS -> Text
getJS (JS t) = t

makeJS
    :: Text
    -> FilePath
    -> Either String JS
makeJS t n = JS t <$ JS.parse (t ^. from packed) n

unsafeMakeJS :: Text -> JS
unsafeMakeJS = JS

instance ToHtml JS where
    toHtmlRaw     = toHtml
    toHtml (JS t) = script_ [] t
