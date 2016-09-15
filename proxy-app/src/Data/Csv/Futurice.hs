-- TODO: deprecate module?
module Data.Csv.Futurice (
    -- * Futurice additions
    FutuCSV,
    FutuCSVOpts,
    -- * Module re-exports
    module Servant.CSV.Cassava,
    module Data.Csv,
    ) where

import Futurice.Prelude ()
import Prelude ()

import Data.Csv
import Servant.CSV.Cassava

type FutuCSV = (CSV', DefaultOpts)

-- TODO: deprecate
type FutuCSVOpts = DefaultOpts
