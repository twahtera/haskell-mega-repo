module Data.Csv.Futurice (
    -- * Futurice additions
    FutuCSV,
    FutuCSVOpts,
    -- * Module re-exports
    module Servant.CSV.Cassava,
    module Data.Csv,
    ) where

import Prelude        ()
import Futurice.Prelude 

import Data.Csv
import Servant.CSV.Cassava

type FutuCSV = (CSV', FutuCSVOpts)

data FutuCSVOpts

instance DecodeOpts FutuCSVOpts where
    decodeOpts _ = decodeOpts (Proxy :: Proxy DefaultDecodeOpts)

instance EncodeOpts FutuCSVOpts where
    encodeOpts _ = encodeOpts (Proxy :: Proxy DefaultEncodeOpts)
