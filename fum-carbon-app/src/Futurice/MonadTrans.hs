{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.MonadTrans (
    MonadTrans' (..),
    ComposeTrans (..),
    ) where

import Control.Monad    (ap, liftM)
import Data.Constraint
import Futurice.Prelude
import Prelude ()

-- instances
import Control.Monad.State.Strict (StateT)
import Futurice.Lucid.Foundation (HtmlT)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class MonadTrans' t where
    lift' :: Monad m => m a -> t m a
    transform' :: Monad m :- Monad (t m)

-------------------------------------------------------------------------------
-- ComposeTrans
-------------------------------------------------------------------------------

newtype ComposeTrans (t :: (* -> *) -> * -> *)
                     (r :: (* -> *) -> * -> *)
                     (m :: * -> *)
                     (a :: *) =
    ComposeTrans { getComposeTrans :: t (r m) a }

instance (MonadTrans' t, MonadTrans' r) => MonadTrans' (ComposeTrans t r) where
    lift' = liftComposeTrans
    transform' = transformComposeTrans

instance (MonadTrans' t, MonadTrans' r, Monad m) => Monad (ComposeTrans t r m) where
    return = ComposeTrans . lift' . return \\
        (transform' :: Monad m :- Monad (r m))
    (>>=) = bindComposeTrans \\ trans
        (transform' :: Monad (r m) :- Monad (t (r m)))
        transform'

bindComposeTrans
      :: Monad (t (r m))
      => ComposeTrans t r m a
      -> (a -> ComposeTrans t r m b)
      -> ComposeTrans t r m b
bindComposeTrans (ComposeTrans m) k = ComposeTrans $ m >>= getComposeTrans . k

instance (MonadTrans' t, MonadTrans' r, Monad m) => Applicative (ComposeTrans t r m) where
    pure = return
    (<*>) = ap

instance (MonadTrans' t, MonadTrans' r, Monad m) => Functor (ComposeTrans t r m) where
    fmap = liftM

liftComposeTrans
    :: forall t r m a. (MonadTrans' t, MonadTrans' r, Monad m)
    => m a -> ComposeTrans t r m a
liftComposeTrans = ComposeTrans . lift' . lift' \\ (transform' :: Monad m :- Monad (r m))

transformComposeTrans
    :: forall t r m. (MonadTrans' t, MonadTrans' r)
    => Monad m :- Monad (ComposeTrans t r m)
transformComposeTrans = Sub Dict

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance MonadTrans' (StateT s) where
    lift' = lift
    transform' = Sub Dict

instance MonadTrans' HtmlT where
    lift' = lift
    transform' = Sub Dict
