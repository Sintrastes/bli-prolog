
-- | The bli prolog server allows the user to make queries
--   via HTTP requests, rather than via command line
--   arguments and the repl.

module Bli.App.Server where

import Network.Shed.Httpd
import qualified Data.ByteString.Lazy.UTF8 as B

-- | A data type to model the types of 
--   requests that can be made to the server
data BliRequest = 
-- | A simple Get request to 
--   make a query and return the
--   results of that query.
     Get  String
-- | Request that an assertion be made.
   | Post String

-- | A data type to model the types of responses
--   that the server can return to clients.
data BliResponse = 
-- | Respond with the appropriate JSON data.
  BliResponse String

-- Note: I'll want to wrap this in my own datatypes above,
-- and then translate it to the "Request" and "Response" datatypes.
requestHandler :: Request -> IO Response
requestHandler = case reqMethod r of
  "GET"     -> return Response { resCode = 0, resHeaders = [], resBody = B.toString $ "test" }
  "POST"    -> return Response { resCode = 0, resHeaders = [], resBody = B.toString $ "test" }
  otherwise -> return Response { resCode = 0, resHeaders = [], resBody = B.toString $ "test" }

-- | Initialize a new bli-prolog server on @port@. 
newServer :: Int -> IO ()
newServer port = initServer port requestHandler