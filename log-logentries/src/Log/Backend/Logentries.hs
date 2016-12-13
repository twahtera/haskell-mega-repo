{-# LANGUAGE OverloadedStrings #-}
-- |
-- @
-- let token = fromJust $ UUID.fromString "12345678-1234-1234-1234-1234567890ab"
-- withLogentriesLogger token $ \logger ->
--     Log.runLogT "test" logger $ Log.logTrace_ "foobar"
-- @
module Log.Backend.Logentries (withLogentriesLogger) where

import Prelude ()
import Prelude.Compat
import Control.Concurrent (threadDelay)
import Control.Exception  (SomeException, handle)
import Data.Foldable      (for_)
import Data.IORef
import Data.UUID.Types    (UUID)
import Network            (HostName)
import Network.Socket
       (AddrInfo (..), AddrInfoFlag (AI_NUMERICSERV), SocketType (Stream),
       connect, defaultHints, getAddrInfo, socket)

import qualified Data.Aeson               as Aeson
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.UUID.Types          as UUID
import qualified Log
import qualified Log.Internal.Logger      as Log
import qualified Network.TLS              as TLS
import qualified Network.TLS.Extra.Cipher as TLS
import qualified System.X509

withLogentriesLogger
    :: UUID                  -- logentries tcp token
    -> (Log.Logger -> IO r)  -- action
    -> IO r
withLogentriesLogger uuid act = do
    ctx <- Log.Backend.Logentries.connect "data.logentries.com" 443
    ref <- newIORef ctx
    logger <- Log.mkBulkLogger "logentries" (logentriesWrite ref) (logentriesSync ref)
    Log.withLogger logger act
  where
    uuidBS = UUID.toASCIIBytes uuid

    logentriesWrite ref msgs = handle retry $ do
        ctx <- readIORef ref
        for_ msgs $ \msg -> do
            send ctx uuidBS
            send ctx " "
            sendLazy ctx (Aeson.encode msg)
            send ctx "\n"
      where
        retry :: SomeException -> IO ()
        retry ex = do
            putStrLn $ "Logentries: unexpecter error: " ++ show ex
            threadDelay $ 10 * 1000000
            -- do we need to cleanup old connction ?
            ctx <- Log.Backend.Logentries.connect "data.logentries.com" 443
            writeIORef ref ctx
            logentriesWrite ref msgs

    logentriesSync ref = do
        ctx <- readIORef ref
        TLS.contextFlush ctx

-------------------------------------------------------------------------------
-- Connection
-------------------------------------------------------------------------------

-- | Connect to the destination using @tcp+ssl@.
connect
    :: HostName
    -> Int
    -> IO TLS.Context
connect hostname port = do
    let hints = defaultHints
          { addrFlags = [AI_NUMERICSERV]
          , addrSocketType = Stream
          }
    addrInfos <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    case addrInfos of
        [] -> fail $ "Cannot resolve host: " ++ hostname
        (addr : _) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            Network.Socket.connect sock (addrAddress addr)
            params <- tlsParams hostname
            ctx <- TLS.contextNew sock params
            TLS.handshake ctx
            pure ctx

-- close :: TLS.Context -> IO ()
-- close = TLS.contextClose

send :: TLS.Context -> BS.ByteString -> IO ()
send ctx bs = TLS.sendData ctx $ BSL.fromChunks [bs]

sendLazy :: TLS.Context -> BSL.ByteString -> IO ()
sendLazy = TLS.sendData

tlsParams :: HostName -> IO TLS.ClientParams
tlsParams hostname = do
    store <- System.X509.getSystemCertificateStore
    let cp = TLS.defaultParamsClient hostname ""
    pure $ cp
        { TLS.clientShared = (TLS.clientShared cp)
            { TLS.sharedCAStore = store
            }
        , TLS.clientSupported = (TLS.clientSupported cp)
            { TLS.supportedCiphers = TLS.ciphersuite_strong
            }
        }
