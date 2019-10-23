
module Bli.App.Config.Executables where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List.Split
import Data.List
import Control.Monad
import Control.Applicative

-- | Template for getting the name of the compiler executable from cabal file
getCompilerNameFromCabal :: Q Exp
getCompilerNameFromCabal = join $ runIO $
     do compilerName <- (\x -> snd $ splitAt 11 x) 
                    <$> (!! 1)
                    <$> filter (\line -> isPrefixOf "executable" line) 
                    <$> splitOn "\n" 
                    <$> readFile "bli-prolog.cabal"
        return [e| compilerName |]


-- | Template for getting the name of the interpreter executable from cabal file
getInterpreterNameFromCabal :: Q Exp
getInterpreterNameFromCabal = join $ runIO $
     do lines <- splitOn "\n" <$> readFile "bli-prolog.cabal"
        let interpName = (\x -> snd $ splitAt 11 x) $
                  (\x -> x !! 0) $
                  filter (\line -> isPrefixOf "executable" line) $ lines
        return [e| interpName |]