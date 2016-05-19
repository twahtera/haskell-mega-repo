{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoOverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Caching where

import PlanMill.Internal.Prelude
import Prelude                   ()

import Data.Binary          (encode)
import Data.Binary.Tagged   (taggedDecodeFileOrFail, taggedEncodeFile)
import Data.Constraint      (Dict (..))
import Data.Digest.Pure.SHA (sha256, showDigest)

import Control.Monad.PlanMill
import PlanMill.Types

newtype CachingT m a = CachingT { runCachingT :: m a }

instance Functor m => Functor (CachingT m) where
    fmap f (CachingT x) = CachingT (fmap f x)

instance Applicative m => Applicative (CachingT m) where
    pure = CachingT . pure
    CachingT f <*> CachingT x = CachingT (f <*> x)
    CachingT x *> CachingT y = CachingT (x *> y)

instance (Applicative m, Monad m) => Monad (CachingT m) where
    return = pure
    (>>) = (*>)
    CachingT x >>= f = CachingT $ x >>= runCachingT . f

instance (Applicative m, MonadIO m) => MonadIO (CachingT m) where
    liftIO = CachingT . liftIO

instance (Applicative m, MonadThrow m) => MonadThrow (CachingT m) where
    throwM = CachingT . throwM

instance (Applicative m, MonadCatch m) => MonadCatch (CachingT m) where
    catch (CachingT a) handler =  CachingT $ catch a (runCachingT . handler)

class (MonadPlanMillC m a, Binary a, HasStructuralInfo a, HasSemanticVersion a)
     => CachingTC m a
instance (MonadPlanMillC m a, Binary a, HasStructuralInfo a, HasSemanticVersion a)
     => CachingTC m a

instance ( ForallFSymbol (MonadPlanMillC m) e
         , ForallFSymbol Binary e
         , ForallFSymbol HasStructuralInfo e
         , ForallFSymbol HasSemanticVersion e
         )
    => ForallFSymbol (CachingTC m) e
  where
    instFSymbol _ f s =
        case proxies of
            (Dict, Dict, Dict, Dict) -> Dict
      where
        proxies =
            ( instFSymbol (Proxy :: Proxy (MonadPlanMillC m)) f s
            , instFSymbol (Proxy :: Proxy Binary) f s
            , instFSymbol (Proxy :: Proxy HasStructuralInfo) f s
            , instFSymbol (Proxy :: Proxy HasSemanticVersion) f s
            )

-- | Caches get requests
instance (Applicative m, MonadPlanMill m, MonadIO m, MonadCatch m)
    => MonadPlanMill (CachingT m) where
    type MonadPlanMillC (CachingT m) = CachingTC m

    planmillAction p@(PlanMillGet qs ps) =
        CachingT $ cached path $ planmillAction p
      where
        path = cacheFile ("get", qs, ps)

    planmillAction p@(PlanMillPagedGet qs ps) =
        CachingT $ cached path $ planmillAction p
      where
        path = cacheFile ("get-paged", qs, ps)

    planmillAction p@(PlanMillPost _ _) =
        CachingT $ planmillAction p

cacheFile :: Binary a => a -> FilePath
cacheFile = (".cache/" ++) .showDigest . sha256 . encode

cached :: ( Applicative m, MonadIO m, MonadCatch m
          , Binary a, HasStructuralInfo a, HasSemanticVersion a
          )
       => FilePath  -- ^ Cache file
       -> m a       -- ^ Action to perform on cache miss
       -> m a
cached path action = do
    e <- liftIO (taggedDecodeFileOrFail path) `catch` onIOError (Left undefined)
    case e of
        Right x -> return x
        Left _  -> do
            x <- action
            liftIO $ taggedEncodeFile path x
            pure x

onIOError :: Monad m => a -> IOError -> m a
onIOError x _ = return x
