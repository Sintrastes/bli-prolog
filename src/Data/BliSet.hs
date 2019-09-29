
module Data.BliSet where

-- | Generic interface for a Set-like container.
class BliSet t where
  insert :: a -> t a -> t a
  tryRemove :: a -> t a -> Either (t a) (t a)
  lookup :: a -> t a -> Maybe a