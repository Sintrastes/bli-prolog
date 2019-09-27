
-- | The bli prolog server allows the user to make queries
--   via HTTP requests, rather than via command line
--   arguments and the repl.

module Bli.App.Server (newServer) where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.HTTP.Types (status200, badRequest400)
import Blaze.ByteString.Builder (copyByteString)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as B
import Data.Text (pack)
import Data.Monoid
import Data.Text.Encoding
import Data.String
import System.Directory
import Control.Monad
import Prolog.Analysis

-- | A data type to model the types of 
--   requests that can be made to the server
data BliRequest = 
-- | A simple Get request to 
--   make a query and return the
--   results of that query.
     MakeQuery B.ByteString
-- | Request that an assertion be made.
   | MakeAssertion B.ByteString

-- | A data type to model the types of responses
--   that the server can return to clients.
data BliResponse = 
  -- | Response to return when 
     SyntaxError InvalidClause
  -- | Response to successful query
   | QuerySuccess BU.ByteString

parseRequest :: Request -> IO (Maybe BliRequest)
parseRequest req 
    | (method == "GET") && (path == [pack "query"]) 
        = do body <- body'
             return $ Just $ MakeQuery body
    | otherwise = return $ Nothing
  where path   = pathInfo req
        method = requestMethod req
        body'  = strictRequestBody req

processResponse :: Maybe BliResponse -> Response
processResponse (Just (SyntaxError err)) = responseBuilder badRequest400 [] "Syntax error"
processResponse (Just (QuerySuccess response)) = jsonResponse $ byteString response
processResponse Nothing = responseBuilder badRequest400 [] "Bad request"

-- Note: I'll want to wrap this in my own datatypes above,
-- and then translate it to the "Request" and "Response" datatypes.
requestHandler :: BliRequest -> IO BliResponse
requestHandler r = undefined --case reqMethod r of

-- | Initialize a new bli-prolog server on @port@. 
newServer :: Int -> IO ()
newServer port = do
  homeDir <- getHomeDirectory
  -- Get keys and certificates.
  let tSet = tlsSettings (homeDir ++ "/.bedelibry/prolog-server/server.crt") 
                         (homeDir ++ "/.bedelibry/prolog-server/server.key")
  -- Run the server on the given port.
  runTLS tSet (setPort port defaultSettings) app

-- Stuff imported from test project

-- Sample app. Will want to change this later.
app req respond = respond $
  case requestMethod req of
    "GET" -> 
      case pathInfo req of
        ["yay"] -> textResponse $ byteString $ fromString $ (BU.toString username ++ "\n" ++ BU.toString password)
          where Just username = lookup "username" (requestHeaders req)
                Just password = lookup "password" (requestHeaders req)
        otherwise -> textResponse $ byteString $ mconcat $ map encodeUtf8 (pathInfo req)
    "POST" -> textResponse "POST"
    otherwise -> textResponse "Method not supported."

-- | Helper function to return a pre-formatted text response.
textResponse str = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ str

-- | Helper function to return a pre-formatted json response.
jsonResponse str = responseBuilder status200 [ ("Content-Type", "text/json") ] $ str