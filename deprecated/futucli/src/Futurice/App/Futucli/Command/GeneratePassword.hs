module Futurice.App.Futucli.Command.GeneratePassword (generatePassword) where

import Futurice.Prelude

import Control.Monad (liftM)
import Control.Monad.CryptoRandom
import Crypto.Random.DRBG (HmacDRBG)

type G = HmacDRBG

chars :: String
chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "!@#?-_,."

generatePassword :: Int -> IO ()
generatePassword l = do
    g <- newGenIO :: IO G
    p <- either throwM pure $ evalCRand (generatePassword' l) $ g
    putStrLn p

generatePassword' :: Int -> CRand G GenError String
generatePassword' l = sequence $ replicate l (element chars)

element :: MonadCRandomR e m => [a] -> m a
element list = (list !!) `liftM` getCRandomR (0, length list - 1)
