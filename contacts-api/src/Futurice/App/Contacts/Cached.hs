{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.Contacts.Cached (
    -- * Memory
    memoryCached,
    -- * Disk
    diskCached
    ) where

import Futurice.Prelude
import Prelude          ()

import Control.Concurrent.STM
import Control.DeepSeq        (force)
import Control.Monad.Catch    (try)
import Data.Binary.Orphans    as Binary
import Data.Binary.Tagged
import Data.Digest.Pure.SHA
import Data.Time              (NominalDiffTime, diffUTCTime, getCurrentTime)
import System.Directory
import System.FilePath

data SP a b = SP !a !b

memoryCached :: forall a. NFData a
             => NominalDiffTime   -- ^ TTL
             -> IO a              -- ^ Cached action
             -> IO (IO a)
memoryCached ttl mx = do
    slot <- newTVarIO Nothing :: IO (TVar (Maybe (SP a UTCTime)))
    return $ do
        now <- getCurrentTime
        r <- atomically $ do
            r <- readTVar slot
            case r of
                Nothing -> pure Nothing
                Just (SP x stamp) ->
                    pure $ Just $ SP x $ now `diffUTCTime` stamp < ttl

        let perform :: IO a
            perform = do
                x <- mx
                x' <- evaluate $ force x
                atomically $ writeTVar slot (Just (SP x' now))
                pure x'

        case r of
            Nothing           -> perform
            Just (SP x True)  -> pure x
            Just (SP _ False) -> perform


binaryDigest :: Binary a => a -> String
binaryDigest = showDigest . sha1 . Binary.encode

diskCached :: (Binary a, HasStructuralInfo a, HasSemanticVersion a, Binary b)
       => b -> IO a -> IO a
diskCached key mx = do
    e <- tryIO (taggedDecodeFileOrFail path)
    case e of
        Right (Right x) -> return x
        _  -> do x <- mx
                 createDirectoryIfMissing True $ takeDirectory path
                 taggedEncodeFile path x
                 return x
  where
    (a:b:c:d:rest) = binaryDigest key
    path = ".cache/" <> [a,b] <> "/" <> [c,d] <> "/" <> rest

tryIO :: IO a -> IO (Either IOError a)
tryIO = try
