
-- | The bli prolog server allows the user to make queries
--   via HTTP requests, rather than via command line
--   arguments and the repl.

module Bli.App.Server where

-- | A data type to model the types of 
--   requests that can be made to the server
data Request = 
-- | A simple Get request to 
--   make a query and return the
--   results of that query.
     Get  String
-- | Request that an assertion be made.
   | Post String

-- | A data type to model the types of responses
--   that the server can return to clients.
data Response = 
-- | Respond with the appropriate JSON data.
  Response String