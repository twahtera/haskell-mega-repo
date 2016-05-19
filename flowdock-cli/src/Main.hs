{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Prelude        ()
import Prelude.Compat

import Control.Lens
import Control.Monad.Catch      (MonadThrow (..))
import Control.Monad.Http       (HttpT (..), MonadHttp (..), httpLbs)
import Control.Monad.IO.Class   (MonadIO (..))
import Data.Aeson               (FromJSON, Value, eitherDecode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Char                (isSpace)
import Data.Maybe               (fromJust, maybeToList)
import Data.Tagged              (Tagged, untag)
import Network.HTTP.Client      (Request, newManager, responseBody)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Options.Applicative

import qualified Chat.Flowdock.REST   as FD
import qualified Data.ByteString.Lazy as LBS

import Text.PrettyPrint.ANSI.Leijen.AnsiPretty

stringTrim :: String -> String
stringTrim = r . ltrim . r . ltrim
  where ltrim = dropWhile isSpace
        r = reverse

readAuthToken :: IO FD.AuthToken
readAuthToken = do
  contents <- readFile ".flowdock-rest-api-token"
  return $ FD.AuthToken $ stringTrim contents

data Command a = Command
  { _cmdRequest :: Tagged a Request
  , _cmdJson    :: Bool
  }

data SomeCommand where
  SomeCommand :: (FromJSON a, AnsiPretty a) => Command a -> SomeCommand

throwDecode :: (MonadThrow m, FromJSON a) => LBS.ByteString -> m a
throwDecode bs = case eitherDecode bs of
  Right x   -> return x
  Left err  -> error $ "throwDecode: " <> err

commandM :: forall a m . (FromJSON a, AnsiPretty a, MonadThrow m, MonadIO m, MonadHttp m) => Command a -> m ()
commandM (Command req outputJson) = do
  token <- liftIO readAuthToken
  let req' = FD.authenticateRequest token $ untag req
  res <- httpLbs req'
  -- liftIO $ print res
  if outputJson
    then do jsonRes <- throwDecode (responseBody res) :: m Value
            liftIO $ LBS.putStr $ encodePretty jsonRes
    else do valueRes <- throwDecode (responseBody res) :: m a
            liftIO $ putDoc $ ansiPretty valueRes
            liftIO $ putChar '\n'

httpIO :: HttpT IO a -> IO a
httpIO a = do
  mgr <- newManager tlsManagerSettings
  flip runHttpT mgr $ a

paramArgument :: Mod ArgumentFields String -> Parser (FD.ParamName a)
paramArgument = fmap FD.mkParamName . strArgument

commands :: Parser SomeCommand
commands = subparser $ mconcat
  [ command "flows"         (info (helper <*> (SomeCommand <$> flowsCmd))         (progDesc "List flows"))
  , command "all-flows"     (info (helper <*> (SomeCommand <$> allFlowsCmd))      (progDesc "List all flows"))
  , command "flow"          (info (helper <*> (SomeCommand <$> flowCmd))          (progDesc "Get a flow"))
  , command "messages"      (info (helper <*> (SomeCommand <$> messagesCmd))      (progDesc "List messages"))
  , command "users"         (info (helper <*> (SomeCommand <$> usersCmd))         (progDesc "List all users"))
  , command "flow-users"    (info (helper <*> (SomeCommand <$> flowUsersCmd))     (progDesc "List flow users"))
  , command "org-users"     (info (helper <*> (SomeCommand <$> orgUsersCmd))      (progDesc "List an organization's users"))
  , command "organisations" (info (helper <*> (SomeCommand <$> organisationsCmd)) (progDesc "List organisations"))
  , command "organisation"  (info (helper <*> (SomeCommand <$> organisationCmd))  (progDesc "Get an organisation"))
  ]
  where
    mkCmd :: Parser (FD.ApiUrl a) -> Parser (Command a)
    mkCmd urlParser = mkCmd' requestParser
      where requestParser = FD.parseApiUrl <$> urlParser

    mkCmd' :: Parser (Maybe (Tagged a Request)) -> Parser (Command a)
    mkCmd' requestParser = Command <$> (fromJust <$> requestParser)
                                   <*> switch (long "json" <> help "Whether to output raw json")

    flowsCmd          = mkCmd (pure FD.flowsUrl)
    allFlowsCmd       = mkCmd (pure FD.allFlowsUrl)
    flowCmd           = mkCmd (FD.flowGetUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    messagesCmd       = mkCmd' (FD.messagesRequest <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW") <*> parseMessageOptions)
      where parseMessageOptions = messageOptions <$> optional (option (eitherReader ev) (long "event" <> metavar "EVENT" <> help "Filter messages by event type."))
                                                 <*> optional (option auto (long "limit" <> metavar "LIMIT" <> help "Maximum number of messages to return."))
                                                 <*> optional (option auto (long "until" <> metavar "MSGID" <> help "Get messages leading to a message id."))
            ev e = maybe (Left ("Unknown event: " <> e)) Right (FD.messageEventFromString e)
    usersCmd          = mkCmd (pure FD.usersUrl)
    flowUsersCmd      = mkCmd (FD.flowUsersUrl <$> paramArgument (metavar "ORG") <*> paramArgument (metavar "FLOW"))
    orgUsersCmd       = mkCmd (FD.organisationUsersUrl <$> paramArgument (metavar "ORG"))
    organisationsCmd  = mkCmd (pure FD.organisationsUrl)
    organisationCmd   = mkCmd (FD.organisationUrl <$> paramArgument (metavar "ORG"))

messageOptions :: Maybe FD.MessageEvent -> Maybe Int -> Maybe Integer -> FD.MessageOptions
messageOptions event limit untilId =
    FD.defMessageOptions
        & FD.msgOptEvents .~ maybeToList event
        & FD.msgOptLimit .~ limit
        & FD.msgOptUntilId .~ (FD.mkIdentifier <$> untilId)

main' :: SomeCommand -> IO ()
main' (SomeCommand cmd) = httpIO $ commandM cmd

main :: IO ()
main = execParser opts >>= main'
  where
    opts = info (helper <*> commands)
      ( fullDesc
     <> progDesc "Try --help if unsure"
     <> header "flowdock-rest-cli - a test for flowdock-rest" )
