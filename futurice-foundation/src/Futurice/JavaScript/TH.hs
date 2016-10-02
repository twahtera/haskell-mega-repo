{-# LANGUAGE TemplateHaskell #-}
module Futurice.JavaScript.TH (
    embedJS,
    ) where

import Futurice.Prelude
import Language.Haskell.TH
       (Exp, Lit (StringL), Q, appE, litE, runIO)
import Language.Haskell.TH.Syntax           (qAddDependentFile)
import Futurice.JavaScript
import Prelude ()

import qualified Data.Text.IO as T

-- | Create 'JS' from a file, compile-time verifying it can be parsed.
--
-- > $(embedJS "supersource.js")
embedJS :: FilePath -> Q Exp
embedJS fp = do
    qAddDependentFile fp
    contents <- runIO $ T.readFile fp
    let literal = StringL $! contents ^. from packed
    case makeJS contents fp of
        Left  err -> fail $ "embedJS " <> fp <> " -- " <> err
        Right _js -> [| unsafeMakeJS . fromString |] `appE` litE literal
