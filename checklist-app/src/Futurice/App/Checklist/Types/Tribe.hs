{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.Checklist.Types.Tribe (
    Tribe,
    HasValidTribes,
    ValidTribes,
    validTribes,
    foldedValidTribes,
    withTestValidTribes,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Lens              (Fold, contains, folding, to)
import Data.Aeson.Compat         (withText)
import Data.Reflection           (Given (..), give)
import Futurice.EnvConfig        (FromEnvVar (..))
import Futurice.Generics
import Futurice.Lucid.Foundation (ToHtml (..))
import Text.Regex.Applicative    (few, match, psym, sym)

import qualified Data.Set        as Set
import qualified Test.QuickCheck as QC

-- | An opaque type around 'Text'.
newtype Tribe = Tribe Text
  deriving (Eq, Ord, Show, Typeable, Generic)

getTribe :: Tribe -> Text
getTribe (Tribe t) = t

instance NFData Tribe

instance Arbitrary Tribe where
    arbitrary = Tribe <$> QC.elements tribes

instance ToJSON Tribe where
    toJSON = toJSON . getTribe
    toEncoding = toEncoding . getTribe

instance HasValidTribes => FromJSON Tribe where
    parseJSON = withText "Tribe" $ \t -> case given of
        ValidTribes ts
            | ts ^. contains t -> pure $ Tribe t
            | otherwise        -> fail $ "Unknown tribe " ++ t ^. unpacked

instance ToHtml Tribe where
    toHtml = toHtml . getTribe
    toHtmlRaw = toHtml

instance ToHttpApiData Tribe where
    toUrlPiece = getTribe

-------------------------------------------------------------------------------
-- ValidTribes
-------------------------------------------------------------------------------

-- | We could use 'Reifies', but that would require to
-- parametrise everything over a type configuring valid
-- tribes. We cut a corner, and use `given`.
type HasValidTribes = Given ValidTribes

-- | An opaque types, use 'foldedValidTribes' to access.
newtype ValidTribes = ValidTribes (Set Text)
  deriving (Show)

validTribes :: HasValidTribes => ValidTribes
validTribes = given

instance FromEnvVar ValidTribes where
    fromEnvVar s = ValidTribes . Set.fromList . map (view packed)
        <$> match regex s
      where
        tribe = few (psym (/= ','))
        regex = (:) <$> tribe <*> many (sym ',' *> tribe)

foldedValidTribes :: Fold ValidTribes Tribe
foldedValidTribes = folding (\(ValidTribes ts) -> ts ^.. folded . to Tribe)

-- | For testing only.
withTestValidTribes :: (HasValidTribes => r) -> r
withTestValidTribes = give $
    ValidTribes $ Set.fromList tribes

-- Arbitrary tribes
tribes :: [Text]
tribes = ["Earth", "In Space"]
