
module Bli.Prolog.Modules where

--
-- | Implementation of the module system for bli prolog.
--

import Data.Bli.Prolog.Ast
import Bli.Prolog.Parser
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec 
import Control.Monad
import Control.Monad.Bli
import Control.Monad.Bli.Pure (liftFromPure)
import Bli.App.Config (getBliModuleData)
import Data.Yaml
import Bli.Util (groupClauses)
import Control.Monad.IO.Class
import qualified Data.BliSet as BliSet
import Data.List

subset xs ys = all (\x -> x `elem` ys) xs

-- | Helper function to load a bli program
--   into the given context.
--
--   Note: I might want to put this somewhere else later.
loadBliProgramFromFile :: String -> Bli ()
loadBliProgramFromFile filePath = do
  maybeModuleData <- liftIO $ getBliModuleData
  case maybeModuleData of
    Nothing -> error "Error parsing module file."
    Just modData -> do 
      maybeProg <- getBliProgramFromFile filePath
      case maybeProg of
        Nothing -> error "Error loading file"
        Just prog -> do 
          
          case BliSet.lookup (\(a,b) -> isSuffixOf filePath b) modData of
            Nothing -> go prog [] modData
            Just (modName,_) -> go prog [modName] modData
 where go prog loadedMods modData = do
          let (types, 
               relations,
               entities,
               clauses,
               moduleNames) = groupClauses prog

          -- Add everything to the local context, and get the result of adding everything.
          newTypesRes  <- newTypes types
          newRelnsRes  <- newRelations relations
          newEntsRes   <- newEntities entities
          newFactsRes  <- newFacts clauses

          -- Note: I need to do some typechecking here somewhere.
          -- Also, I think that everything needs to be loaded first before typechecking can occur.
          -- So, I'm not sure how that should be implemented.

          case newTypesRes && newRelnsRes && newEntsRes && newFactsRes of
            False -> error "Error loading program."
            True  -> do
              case moduleNames `subset` loadedMods of
                True  -> return ()
                False -> do
                  mapM_ (\modName -> do
                          let maybeModPath = BliSet.lookup (\(x,y) -> x==modName) modData
                          case maybeModPath of
                            Nothing -> error "Module has not been declared."
                            Just (_,modPath) -> do
                              maybeLoadedMod <- getBliProgramFromFile modPath
                              case maybeLoadedMod of
                                Nothing -> error "Error loading module."
                                Just loadedMod -> do
                                  go loadedMod (modName:loadedMods) modData
                       ) moduleNames

-- | Helper function to load a Bli program from a file.
--   Returns a boolean flag to indicate success/failure.
-- 
--   Note: Again, I may want to put this somewhere else later.
getBliProgramFromFile :: String -> Bli (Maybe BliProgram)
getBliProgramFromFile filePath = undefined 
-- ^ FOR DEBUGGING
{- do
  fileContents <- liftIO $ readFile filePath
  parseResult <- liftFromPure $ parseBliPrologProgram fileContents
  case parseResult of
    Left e -> error $ show e -- return $ Nothing
    Right program -> return $ Just program
-}