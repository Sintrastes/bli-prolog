
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
import Prolog.Parser
import Control.Applicative
import Control.Monad.Bli

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
   | AssertionSuccess

parseRequest :: Request -> IO (Maybe BliRequest)
parseRequest req 
    | (method == "GET") && (path == [pack "query"]) 
        = do body <- body'
             return $ Just $ MakeQuery body
    | otherwise = return $ Nothing
  where path   = pathInfo req
        method = requestMethod req
        body'  = strictRequestBody req

processResponse :: Maybe BliResponse -> IO Response
processResponse (Just (SyntaxError err)) = return $ responseBuilder badRequest400 [] "Syntax error"
processResponse (Just (QuerySuccess response)) = return $ jsonResponse $ byteString response
processResponse (Just AssertionSuccess) = return $ responseBuilder status200 [] ""
processResponse Nothing = return $ responseBuilder badRequest400 [] "Bad request"

-- This is where the magic happens.
requestHandler :: Maybe BliRequest -> IO (Maybe BliResponse)
requestHandler (Just (MakeQuery query)) 
  = case (parseBliCommand $ BU.toString $ B.toStrict $ query) of 
      Left err -> return $ Just $ SyntaxError $ undefined
      Right val -> undefined
requestHandler (Just (MakeAssertion assertion)) = undefined
-- If we recieve an unsupported request, return the appropriate
-- response.
requestHandler Nothing = return Nothing

-- | Initialize a new bli-prolog server on @port@. 
newServer :: Int -> Bli ()
newServer port = do
  homeDir <- io $ getHomeDirectory
  -- Get keys and certificates.
  let tSet = tlsSettings (homeDir ++ "/.bedelibry/prolog-server/server.crt") 
                         (homeDir ++ "/.bedelibry/prolog-server/server.key")
  -- Run the server on the given port.
  io $ runTLS tSet (setPort port defaultSettings) app

-- | Warp application for our server.
app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req respond = 
     respond
 =<< processResponse
 =<< requestHandler
 =<< parseRequest req

-- | Helper function to return a pre-formatted text response.
textResponse str = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ str

-- | Helper function to return a pre-formatted json response.
jsonResponse str = responseBuilder status200 [ ("Content-Type", "text/json") ] $ str