
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
import Bli.Prolog.Typechecking hiding (InvalidClause(..))
import Bli.Prolog.Parser
import Bli.Prolog.Parser.Cli
import Control.Applicative
import Control.Monad.Bli
import Control.Monad.Bli.Common
import Control.Monad.Bli.Conversions
import Control.Monad.IO.Class
import Data.IORef

import qualified Control.Monad.Bli.Pure as Pure

import Bli.App
import Bli.App.Api

parseRequest :: Request -> Bli (Maybe BliRequest)
parseRequest req 
    | (method == "GET") && (path == []) 
        = do body <- liftIO $ body'
             let Just query = join $ lookup (BU.fromString "query") params
             -- liftIO $ print query
             return $ Just $ MakeQuery $ BU.toString query
    | otherwise = return $ Nothing
  where path   = pathInfo req
        params = queryString req
        method = requestMethod req
        body'  = strictRequestBody req

processResponse :: Maybe BliResponse -> Bli Response
processResponse (Just (BliResponse (SyntaxError err))) = return $ responseBuilder badRequest400 [] "Syntax error"
processResponse (Just (BliResponse (QuerySuccess response))) = return $ jsonResponse $ byteString $ BU.fromString $ show response
processResponse (Just (BliResponse (AssertionSuccess _))) = return $ responseBuilder status200 [] "Assertion success"
processResponse Nothing = return $ responseBuilder badRequest400 [] "Bad request"

-- This is where the magic happens.
-- Note: This implementatio seems a bit inefficent at the moment.
-- There are a lot of cases here that will never be reached.
requestHandler :: Maybe BliRequest -> Bli (Maybe BliResponse)
requestHandler (Just (MakeQuery query)) 
  = do parseResult <- liftFromPure $ parseBliCommandTyped query
       case parseResult of 
         Left err -> return $ Just $ BliResponse $ SyntaxError $ show err
         Right command -> do
          results <- processBliCommand command
          return $ Just $ BliResponse (head results)
          {- do
           results <- processBliCommand command
           -- Todo: This should handle all errors, not just the first one.
           case head results of
             Result_QueryFail_AtomsNotInSchema atoms -> do
                 return $ Just $ QuerySuccess "Query fail: Atoms not in schema" -- "Query fail, replace me."
             Result_QueryFail_BoundVarNotInBody -> do
                 return $ Just $ QuerySuccess "Query fail: Bound var not in body." -- "Query fail, replace me."
             Result_QuerySuccess solutions -> do
                 return $ Just $ QuerySuccess (show solutions)
        -- These will never happen.
             Result_AssertionSuccess -> do
                 return $ Just $ AssertionSuccess
             Result_AssertionFail_AtomsNotInSchema atoms -> do
                 return $ Just $ QuerySuccess "Assertion fail" -- "replace me." -}
requestHandler (Just (MakeAssertion assertion))
  = do parseResult <- liftFromPure $ parseBliCommandTyped assertion
       case parseResult of 
         Left err -> return $ Just $ BliResponse $ SyntaxError $ show err -- "Some error. Replace me!"
         Right command -> do
          results <- processBliCommand command
           -- Todo: This should handle all errors, not just the first one.
          return $ Just $ BliResponse (head results)

         {- Left err -> return $ Just $ SyntaxError $ BoundVarNotInBody -- "replace me."
         Right command -> do
           results <- processBliCommand command
           -- Todo: This should handle all errors, not just the first one.
           case head results of
           -- These will never happen
             Result_QueryFail_AtomsNotInSchema atoms -> do
                 return $ Just $ QuerySuccess "Query fail: Atoms not in schema." -- "replace me."
             Result_QueryFail_BoundVarNotInBody -> do
                 return $ Just $ QuerySuccess "Query fail: Bound var not in body." -- "replace me."
             -- These will happen.
             Result_QuerySuccess solutions -> do
               return $ Just $ QuerySuccess (show solutions) -- "replace me."
             Result_AssertionSuccess -> do
               return $ Just $ AssertionSuccess
             Result_AssertionFail_AtomsNotInSchema atoms -> do
               return $ Just $ QuerySuccess "Assertion fail: Atoms not in schema." -- "replace me." -}
-- If we recieve an unsupported request, return the appropriate
-- response.
requestHandler Nothing = return Nothing

-- | Initialize a new bli-prolog server on @port@. 
newServer :: Int -> Bli ()
newServer port = do
  homeDir <- liftIO $ getHomeDirectory
  ioRef   <- getIORefOfStore
  -- Get keys and certificates.
  let tSet = tlsSettings (homeDir ++ "/.bedelibry/prolog-server/server.crt") 
                         (homeDir ++ "/.bedelibry/prolog-server/server.key")
  -- Run the server on the given port.
  -- liftIO $ runTLS tSet (setPort port defaultSettings) ( \x -> \y -> app x y ioRef)
  liftIO $ run port ( \x -> \y -> app x y ioRef)

-- | Warp application for our server.
app :: Request -> (Response -> IO ResponseReceived) -> IORef (BliStore FactContainer RelationContainer EntityContainer TypeContainer AliasDatastructure) -> IO ResponseReceived
app req respond = unwrap $  
     (liftIO . respond)
 =<< processResponse
 =<< requestHandler
 =<< parseRequest req

-- | Helper function to return a pre-formatted text response.
textResponse str = responseBuilder status200 [ ("Content-Type", "text/plain") ] $ str

-- | Helper function to return a pre-formatted json response.
jsonResponse str = responseBuilder status200 [ ("Content-Type", "text/json") ] $ str