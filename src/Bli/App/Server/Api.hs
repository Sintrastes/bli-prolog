
module Bli.App.Server.Api where

import Servant.API

{-
type BliPrologServerApi = 
      "query"  :> ReqBody '[JSON] JSONQuery  :> Get '[JSON] [Solution]
 :<|> "assert" :> ReqBody '[JSON] JSONAssert :> Get '[JSON] [JSONResponse]
-}