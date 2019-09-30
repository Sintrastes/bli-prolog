
module Bli.App.Config.Version where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List.Split
import Data.List
import Control.Monad
import Control.Applicative

-- | Template for getting version number from cabal file
getVersionFromCabal :: Q Exp
getVersionFromCabal = join $ runIO $
     do version <- fmap (filter (\x -> x /= ' ')) 
               <$> fmap (\x -> snd $ splitAt 8 x) 
               <$> find (\line -> isPrefixOf "version:" line) 
               <$> splitOn "\n" 
               <$> readFile "bli-prolog.cabal"
        return [e| version |]
