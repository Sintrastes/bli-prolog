
import Data.Functor.Identity
import Control.Appliative

data Foo' m = Foo' {
  foo :: m String,
  bar :: Maybe Int
}

type MaybeFoo = Foo' Maybe
type Foo = Foo' Identity


a :: MaybeFoo
a = Foo' {
  foo = Just "a",
  bar = Nothing
}

b :: MaybeFoo
b = Foo' {
  foo = Just "b",
  bar = Just 42
}




MaybeFoo -> Maybe Foo