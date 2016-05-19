{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Data source for raw http requests
module Haxl.Extra.IODataSource (
    ioAction,
    initDataSource,
    GenIORequest(..),
    IODataSourceTag(..),
    ) where

import Prelude        ()
import Prelude.Compat

import Control.Concurrent.ParallelIO.Local (parallel_, withPool)
import Data.Hashable                       (Hashable (..))
import Data.Proxy                          (Proxy (..))
import Data.Text                           (Text)
import Data.Typeable                       (Typeable)
import Haxl.Core

-- | Generic IO request.
-- IO actions will be performed in parallel.
-- The @t@ parameter is used for two purposes:
--
--   * value is used to differentiate requests (caching and deduping)
--   * type is used as a parameter for type level functions ('IOEnv').
data GenIORequest t a = GenIORequest t (IOEnv t -> IO a)
    deriving (Typeable)

instance Show t => Show (GenIORequest t a) where
    showsPrec p (GenIORequest tag _) =
        showParen (p > 10) $ showString "GenIORequest " . showsPrec 11 tag

instance Eq t => Eq (GenIORequest t a) where
    (GenIORequest tag _) == (GenIORequest tag' _) = tag == tag'

instance Show t => Show1 (GenIORequest t) where show1 = show

instance Hashable t => Hashable (GenIORequest t a) where
    hashWithSalt salt (GenIORequest tag _) = hashWithSalt salt tag

-- | Generic IO action in 'GenHaxl' monad.
ioAction :: (IODataSourceTag t, Show a, Typeable a)
         => t                    -- ^ Tag
         -> (IOEnv t -> IO a)    -- ^ Action
         -> GenHaxl u a
ioAction tag action = dataFetch (GenIORequest tag action)

-- | A class providing to parametrise different IO actions.
class (Eq t, Show t, Hashable t, Typeable t) => IODataSourceTag t where
    type IOEnv t
    ioSourceName :: proxy t -> Text

-- | Init data source
initDataSource :: Int      -- ^ Pool size
               -> IOEnv t  -- ^ Reader state
               -> IO (State (GenIORequest t))
initDataSource poolSize s = pure (IODataState poolSize s)

instance IODataSourceTag t => StateKey (GenIORequest t) where
    data State (GenIORequest t) = IODataState Int (IOEnv t)

instance IODataSourceTag t => DataSourceName (GenIORequest t) where
    dataSourceName _ = ioSourceName (Proxy :: Proxy t)

instance IODataSourceTag t => DataSource u (GenIORequest t) where
    fetch (IODataState poolSize s) _flags _userEnv blockedFetches =
        SyncFetch $ withPool poolSize $ \pool ->
            parallel_ pool (fmap doOne blockedFetches)
      where
        doOne :: BlockedFetch (GenIORequest t) -> IO ()
        doOne (BlockedFetch (GenIORequest _tag action) v) =
            action s >>= putSuccess v
