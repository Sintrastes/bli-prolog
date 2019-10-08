
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
import Control.Monad.IO.Class

import qualified Control.Monad.Bli.Pure as Pure

import Bli.App
import Bli.App.Api

parseRequest :: Request -> Bli (Maybe BliRequest)
parseRequest req 
    | (method == "GET") && (path == [pack "query"]) 
        = do body <- liftIO $ body'
             return $ Just $ MakeQuery $ BU.toString $ B.toStrict body
    | otherwise = return $ Nothing
  where path   = pathInfo req
        method = requestMethod req
        body'  = strictRequestBody req

processResponse :: Maybe BliResponse -> Bli Response
processResponse (Just (SyntaxError err)) = return $ responseBuilder badRequest400 [] "Syntax error"
processResponse (Just (QuerySuccess response)) = return $ jsonResponse $ byteString $ BU.fromString $ response
processResponse (Just AssertionSuccess) = return $ responseBuilder status200 [] ""
processResponse Nothing = return $ responseBuilder badRequest400 [] "Bad request"

-- This is where the magic happens.
-- Note: This implementatio seems a bit inefficent at the moment.
-- There are a lot of cases here that will never be reached.
requestHandler :: Maybe BliRequest -> Bli (Maybe BliResponse)
requestHandler (Just (MakeQuery query)) 
  = case (parseBliCommandTyped query) of 
      Left err -> return $ Just $ SyntaxError $ BoundVarNotInBody -- "Some error. Replace me!"
      Right command -> do
        result <- Pure.liftFromPure $ processBliCommand command
        case result of
          Result_QueryFail (AtomsNotInSchema atoms) -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "Query fail, replace me."
          Result_QueryFail BoundVarNotInBody -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "Query fail, replace me."
          Result_QuerySuccess solutions -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "Query success, replace me."
     -- These will never happen.
          Result_AssertionSuccess -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
          Result_AssertionFail atoms -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
requestHandler (Just (MakeAssertion assertion))
  = case (parseBliCommandTyped assertion) of 
      Left err -> return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
      Right command -> do
        result <- Pure.liftFromPure $ processBliCommand command
        case result of
       -- These will never happen
          Result_QueryFail (AtomsNotInSchema atoms) -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
          Result_QueryFail BoundVarNotInBody -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
       -- These will happen.
          Result_QuerySuccess solutions -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
          Result_AssertionSuccess -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
          Result_AssertionFail atoms -> do
              return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
-- If we recieve an unsupported request, return the appropriate
-- response.
requestHandler Nothing = return Nothing

-- | Initialize a new bli-prolog server on @port@. 
newServer :: Int -> Bli ()
newServer port = do
  homeDir <- liftIO $ getHomeDirectory
  store   <- getStore
  -- Get keys and certificates.
  let tSet = tlsSettings (homeDir ++ "/.bedelibry/prolog-server/server.crt") 
                         (homeDir ++ "/.bedelibry/prolog-server/server.key")
  -- Run the server on the given port.
  liftIO $ runTLS tSet (setPort port defaultSettings) ( \x -> \y -> (runBliWithStore store) $ app x y)

-- | Warp application for our server.
app :: Request -> (Response -> IO ResponseReceived) -> Bli ResponseReceived
app req respond = 
     (liftIO . respond)
 =<< processResponse
 =<< requestHandler
 =<< parseRequest req

-- | Helper function to return a pre-formatted text response.
textResponse str = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ str

-- | Helper function to return a pre-formatted json response.
jsonResponse str = responseBuilder status200 [ ("Content-Type", "text/json") ] $ str