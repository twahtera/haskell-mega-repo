{-# LANGUAGE OverloadedStrings #-}
-- | Papertrail logging common functionality.
module Papertrail (
    -- * Log designation
    parseLogDestination,
    uriToLogDestination,
    -- * Connection
    Papertrail.connect,
    send,
    close,
    -- * Formatting
    formatMessage,
    Severity (..),
    -- * Testing
    testLog,
    ) where

import Prelude ()
import Prelude.Compat
import Control.Monad ((>=>))
import Data.Text      (Text)
import Data.Time      (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Network        (HostName)
import Network.Socket
       (AddrInfo (..), AddrInfoFlag (AI_NUMERICSERV), SocketType (Stream),
       connect, defaultHints, getAddrInfo, socket)
import Network.URI    (URI (..), URIAuth (..), parseURI)
import Text.Read      (readMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Network.TLS           as TLS
import qualified Network.TLS.Extra.Cipher as TLS

import Papertrail.Certificate

-- | Parse url to act as papertrail log destination:
--
-- >>> parseLogDestination "papertrail://foo.papertrailapp.com:7357"
-- Just ("foo.papertrailapp.com",7357)
parseLogDestination :: String -> Maybe (HostName, Int)
parseLogDestination =  parseURI >=> uriToLogDestination

uriToLogDestination :: URI -> Maybe (HostName, Int)
uriToLogDestination uri
    | uriScheme uri /= "papertrail:" = Nothing
    | otherwise                      = do
        -- host
        uriAuth <- uriAuthority uri
        -- port
        let p0 = uriPort uriAuth
        p <- case p0 of
            (':' : p1) -> readMaybe p1
            _          -> Nothing
        -- combine
        pure (uriRegName uriAuth, p)

-- | Format message based on <https://tools.ietf.org/html/rfc5424 RC5424>
--
-- /Note:/ newline character is appended
--
-- >>> BS.putStr $ formatMessage "mymachine.example.com" "ls" "42" "" NOTICE now "Directory don't exist"
-- <13>1 2016-11-21T06:24:01Z mymachine.example.com ls 42 - - ﻿Directory don't exist
formatMessage
    :: Text -- ^ The HOSTNAME field identifies the machine that originally sent the syslog message.
    -> Text -- ^ The APP-NAME field SHOULD identify the device or application that originated the message.
    -> Text -- ^ PROCID is a value that is included in the message, having no interoperable meaning, except that a change in the value indicates there has been a discontinuity in syslog reporting.
    -> Text -- ^ The MSGID SHOULD identify the type of message.
    -> Severity
    -> UTCTime
    -> Text -- ^ Message
    -> BS.ByteString
formatMessage hn an pid mid sev now msg = mconcat
    -- Excerpt from rfc5424:
    --
    -- SYSLOG-MSG      = HEADER SP STRUCTURED-DATA [SP MSG]
    -- HEADER          = PRI VERSION SP TIMESTAMP SP HOSTNAME SP APP-NAME SP PROCID SP MSGID
    [ "<"
    , pri
    , ">1 "                   -- PRI VERSION SP
    , BS8.pack now'          -- TIMESTAMP
    , " "                    -- SP
    , toBS 255 hn            -- HOSTNAME = NILVALUE / 1*255PRINTUSASCII
    , " "                    -- SP
    , toBS 48 an             -- APP-NAME = NILVALUE / 1*48PRINTUSASCII
    , " "                    -- SP
    , toBS 128 pid           -- PROCID = NILVALUE / 1*128PRINTUSASCII
    , " "                    -- SP
    , toBS 32 mid            -- MSGID = NILVALUE / 1*32PRINTUSASCII
    , " "                    -- SP
    , "[exampleSDID@32473 iut=\"3\" eventSource=\"Application\" eventID=\"1011\"]"                    -- STRUCTURED-DATA
    , " "                    -- SP
    , bom                    -- BOM
    , toBS 1024 msg          -- MSG-UTF8, we truncate message to not be too long.
    , "\n"                   -- message is newline terminated.
    ]
  where
    pri = BS8.pack $ show (8 + fromEnum sev)
    bom = BS.pack [0xEF, 0xBB, 0xBF]
    now' = formatTime defaultTimeLocale "%FT%TZ" now
    toBS m t
        | T.null t  = "-"
        | otherwise = TE.encodeUtf8 $ T.take m t

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
            ctx <- TLS.contextNew sock $ tlsParams hostname
            TLS.handshake ctx
            pure ctx

close :: TLS.Context -> IO ()
close = TLS.contextClose

send :: TLS.Context -> BS.ByteString -> IO ()
send ctx bs = TLS.sendData ctx $ BSL.fromChunks [bs]

tlsParams :: HostName -> TLS.ClientParams
tlsParams hostname =
    let cp0 = TLS.defaultParamsClient hostname "foo"
        cp1 = cp0
            { TLS.clientShared = (TLS.clientShared cp0)
                { TLS.sharedCAStore = papertrailStore
                }
            , TLS.clientSupported = (TLS.clientSupported cp0)
                { TLS.supportedCiphers = TLS.ciphersuite_strong
                }
            }
    in cp1

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

-- | Test implementation.
testLog
    :: String -- ^ papertrail log destination, e.g. @papertrail://foo.papertrail.app:7357@
    -> Text   -- ^ log message
    -> IO ()
testLog d msg = do
    now <- getCurrentTime
    (hostname, port) <- maybe (fail $ "Cannot parse log destination: " ++ d) pure $
        parseLogDestination d
    let bs = formatMessage "test.example.com" "locallog.txt" "42" "" NOTICE now msg
    ctx <- Papertrail.connect hostname port
    send ctx bs
    close ctx

-------------------------------------------------------------------------------
-- Syslog
-------------------------------------------------------------------------------

-- | Severity of the message.
--
-- /Each message Priority also has a decimal Severity level indicator./
--
-- 'Enum' instance encodes to proper integral values.
data Severity 
    = EMERGENCY  -- ^ System is unusable
    | ALERT      -- ^ Take immediate action
    | CRITICAL   -- ^ Severe situations
    | ERROR      -- ^ General Errors
    | WARNING    -- ^ General Warnings
    | NOTICE     -- ^ Normal runtime conditions
    | INFO       -- ^ Information
    | DEBUG      -- ^ Debug messages
 deriving
    (Eq, Ord, Enum, Bounded, Show, Read)
