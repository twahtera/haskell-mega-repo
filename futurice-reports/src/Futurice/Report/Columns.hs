{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings       #-}
module Futurice.Report.Columns (
    -- * Report
    Report (..),
    ReportGenerated,
    -- * Class
    ToColumns (..),
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat         (FromJSON (..), ToJSON (..))
import Futurice.List
import Futurice.Lucid.Foundation (ToHtml (..))
import GHC.TypeLits              (Symbol)

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

-- | The report.
--
-- Report is parameterised over
--
-- * @name@ - the name
-- * @params@ - the report parameteters, e.g. 'ReportGenerated'
-- * @fs@ and @a@ - report actual data
data Report (name :: Symbol) params a = Report
    { reportParams :: !params
    , reportData   :: !a
    }
    deriving (Typeable, Functor, Foldable, Traversable)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class ToColumns a where
    type Columns a :: [*]

    toColumns :: a -> [NP I (Columns a)]

-------------------------------------------------------------------------------
-- Scalars
-------------------------------------------------------------------------------

instance ToColumns () where
    type Columns () = '[]

    toColumns _ = []

-------------------------------------------------------------------------------
-- Containers
-------------------------------------------------------------------------------

instance ToColumns a => ToColumns [a] where
    type Columns [a] = Columns a

    toColumns = concatMap toColumns

instance (ToColumns a, ToColumns b) => ToColumns (a, b) where
    type Columns (a, b) = Append (Columns a) (Columns b)

    toColumns (a, b) = append <$> toColumns a <*> toColumns b

instance ToColumns a => ToColumns (Vector a) where
    type Columns (Vector a) = Columns a

    toColumns = foldMap toColumns

instance (ToColumns k, ToColumns v) => ToColumns (Map k v) where
    type Columns (Map k v) = Append (Columns k) (Columns v)

    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

-- TODO: https://github.com/ekmett/lens/pull/679
instance (Eq k, Hashable k, ToColumns k, ToColumns v) => ToColumns (HashMap k v) where
    type Columns (HashMap k v) = Append (Columns k) (Columns v)

    toColumns = ifoldMap f
      where
        f k v = append <$> toColumns k <*> toColumns v

-------------------------------------------------------------------------------
-- ReportGenerated
-------------------------------------------------------------------------------

-- | Report generation time.
newtype ReportGenerated = ReportGenerated { getReportGenerated :: UTCTime }
    deriving (Typeable)

instance NFData ReportGenerated where
    rnf = rnf . getReportGenerated

instance ToJSON ReportGenerated where
    toJSON = toJSON . getReportGenerated

instance FromJSON ReportGenerated  where
    parseJSON = fmap ReportGenerated . parseJSON

instance ToHtml ReportGenerated where
    toHtml r = do
      "Generated at "
      toHtml . show . getReportGenerated $ r
    toHtmlRaw = toHtml
