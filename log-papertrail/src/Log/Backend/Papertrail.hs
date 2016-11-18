{-# LANGUAGE OverloadedStrings #-}
module Log.Backend.Papertrail (
    -- * Logger
    withPapertrailLogger,
    -- * Utilities
    formatMessage,
    testLog,
    ) where

import Prelude ()
import Prelude.Compat
import Data.Text                 (Text)
import Data.Time
       (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Network
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.URI               (URI (..), URIAuth (..), parseURI)
import Text.Read                 (readMaybe)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Network.TLS           as TLS

-- | TODO: intergrate with @log@
type Logger = ()

withPapertrailLogger :: (Logger -> IO r) -> IO r
withPapertrailLogger f = f ()

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
-- Note: newline character is appended
--
-- >>> BS.putStr $ formatMessage "mymachine.example.com" "ls" "" "ERROR" "Directory don't exist" `mappend` "\n"
-- <165>1 2016-11-18T14:00:00Z mymachine.example.com ls - ERROR - Directory don't exist
formatMessage
    :: UTCTime
    -> Text -- ^ The HOSTNAME field identifies the machine that originally sent the syslog message.
    -> Text -- ^ The APP-NAME field SHOULD identify the device or application that originated the message.
    -> Text -- ^ PROCID is a value that is included in the message, having no interoperable meaning, except that a change in the value indicates there has been a discontinuity in syslog reporting.
    -> Text -- ^ The MSGID SHOULD identify the type of message.
    -> Text -- ^ Message
    -> BS.ByteString
formatMessage now hn an pid mid msg = mconcat
    -- Excerpt from rfc5424:
    --
    -- SYSLOG-MSG      = HEADER SP STRUCTURED-DATA [SP MSG]
    -- HEADER          = PRI VERSION SP TIMESTAMP SP HOSTNAME SP APP-NAME SP PROCID SP MSGID
    [ "<13>1 "               -- PRI VERSION SP, TODO:
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
    , "-"                    -- STRUCTURED-DATA
    , " "                    -- SP
    , bom                    -- BOM
    , toBS 1024 msg          -- MSG-UTF8, we truncate message to not be too long.
    , "\n"                   -- message is newline terminated.
    ]
  where
    bom = BS.pack [0xEF, 0xBB, 0xBF]
    now' = formatTime defaultTimeLocale "%FT%TZ" now
    toBS m t
        | T.null t  = "-"
        | otherwise = TE.encodeUtf8 $ T.take m t

-- | Test implementation
testLog
    :: String -- ^ papertrail log destination, e.g. @papertrail://foo.papertrail.app:7357@
    -> Text   -- ^ log message
    -> IO ()
testLog d msg = do
    now <- getCurrentTime
    (hostname, port) <- maybe (fail $ "Cannot parse log destination: " ++ d) pure $
        parseURI d >>= uriToLogDestination
    let bs = formatMessage now "test.example.com" "locallog.txt" "" "" msg
    let hints = defaultHints
          { addrFlags = [AI_NUMERICSERV]
          , addrSocketType = Stream
          }
    addrInfos <- getAddrInfo (Just hints) (Just hostname) (Just $ show port)
    case addrInfos of
        [] -> fail $ "Cannot resolve host: " ++ hostname
        (addr : _) -> do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock (addrAddress addr)
            sendAll sock bs
            close sock

tlsParams :: HostName -> TLS.ClientParams
tlsParams = undefined

-------------------------------------------------------------------------------
-- Syslog
-------------------------------------------------------------------------------

data Priority
    = DEBUG      -- ^ Debug messages
    | INFO       -- ^ Information
    | NOTICE     -- ^ Normal runtime conditions
    | WARNING    -- ^ General Warnings
    | ERROR      -- ^ General Errors
    | CRITICAL   -- ^ Severe situations
    | ALERT      -- ^ Take immediate action
    | EMERGENCY  -- ^ System is unusable
 deriving
    (Eq, Ord, Enum, Bounded, Show, Read)
