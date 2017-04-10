{-# LANGUAGE FlexibleContexts #-}
module Options.SOP (
    sopCommandParser,
    FromOptions (..),
    module Options.Applicative,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Char                 (isUpper, toLower)
import Data.List                 (intercalate)
import Data.List.CommonPrefix    (CommonPrefix (..), getCommonPrefix)
import Data.List.Split           (keepDelimsL, split, whenElt)
import Data.Time.Parsers         (day)
import Numeric.Interval.NonEmpty (Interval, (...))
import Options.Applicative
import Text.Trifecta             as T (Result (..), parseString, _errDoc)

import qualified Generics.SOP      as SOP
import qualified Generics.SOP.Lens as SOPL

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class FromOptions a where
    optionsParser :: Parser a

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance FromOptions Text where
    optionsParser = view packed <$> strArgument (metavar ":???")

instance FromOptions Int where
    optionsParser = argument auto (metavar ":int")

instance FromOptions Day where
    optionsParser = argument (eitherReader r) (metavar ":day")
      where
        r s = case parseString day mempty s of
            T.Success x -> pure x
            T.Failure ei -> throwError $ show $ _errDoc ei

instance (FromOptions a, Ord a) => FromOptions (Interval a) where
    optionsParser = (...) <$> optionsParser <*> optionsParser

-------------------------------------------------------------------------------
-- generics
-------------------------------------------------------------------------------

hapInjs'_POP :: SOP.SListI xss => SOP.POP f xss -> NP (K (SOP.SOP f xss)) xss
hapInjs'_POP = SOP.hmap (\(K x) -> K $ SOP.SOP x) . SOP.hap SOP.injections . SOP.unPOP

sopCommandParser
    :: forall a. (SOP.All2 FromOptions (SOP.Code a), SOP.Generic a, SOP.HasDatatypeInfo a)
    => Parser a
sopCommandParser
    = subparser $ mconcat
    $ SOP.hcollapse $ SOP.hzipWith f cinfos $ hapInjs'_POP parsers
  where
    cinfos :: NP SOP.ConstructorInfo (SOP.Code a)
    cinfos = SOP.datatypeInfo (Proxy :: Proxy a) ^. SOPL.constructorInfo

    parsers :: SOP.POP Parser (SOP.Code a)
    parsers = SOP.hcpure (Proxy :: Proxy FromOptions) optionsParser

    prefix :: String
    prefix = case cnames of
        []     -> []
        [_]    -> []
        (x:xs) -> getCommonPrefix $ foldMap1 CommonPrefix $ x :| xs
      where
        cnames = SOP.hcollapse $ SOP.hmap (K . view SOPL.constructorName) cinfos

    f :: SOP.ConstructorInfo xs -> K (SOP.SOP Parser (SOP.Code a)) xs -> K (Mod CommandFields a) xs
    f cinfo (K parser) = K $ command
        (dashify $ drop (length prefix) $ cinfo ^. SOPL.constructorName)
        (info (helper <*> parser') mempty)
      where
        parser' = SOP.to <$> SOP.hsequence parser

    -- >>> dashify "FooBar"
    -- 'foo-bar"
    dashify :: String -> String
    dashify
        = map toLower
        . intercalate "-"
        . filter (not . null)
        . split (keepDelimsL $ whenElt isUpper)
