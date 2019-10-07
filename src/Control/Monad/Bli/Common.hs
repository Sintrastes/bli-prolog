
module Control.Monad.Bli.Common where

data BliStore t1 t2 t3 t4 alias = BliStore {
  config :: AppConfig,
  facts  :: t1 Clause,
  relations :: t2 RelDecl,
  entities :: t3 EntityDecl,
  types :: t4 TypeDecl,
  aliases :: alias
}