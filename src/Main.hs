{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}

--
-- | Main entrypoint for the bli-prolog executable.
--

module Main where

import Bli.App
import Bli.App.Config
import Bli.App.Colors
import Bli.App.Json
import Bli.App.Server
import Bli.App.Cli
import Bli.App.Api
import Control.Monad.Bli
import qualified Control.Monad.Bli.Pure as Pure
import System.Console.CmdArgs as CA hiding (program)
import Bli.Prolog.Parser
import Control.Monad (when)
import Data.List (intersperse, isPrefixOf)
import Data.List.Split

main = do
  result <- configureApplication
  -- Make sure version loaded from file successfully.
  case result of 
    Left err -> putStrLn $ (red True) "Error loading command line options. Aborting."
    Right opts -> do
      let versionStr = version opts
      let colorOpts = not $ nocolor opts
      -- If prolog file not specified, start with an empty set of clauses.
      p <- case program opts of
        "" -> return $ Right []
        p  -> error "TODO: Implement this using out new code."
        -- _  -> clausesFromFile $ program opts
      -- If Schema file not specified, start with an empty schema. 
      s <- case schema opts of
        "" -> return $ Right []
        _  -> error "TODO: Implement this using new code." -- schemaFromFile $ schema opts
      -- Handle parse errors for prolog and schema files.
      case (p,s) of 
        (Left (err :: String),_) -> 
          do putStrLn ((red colorOpts "Error") ++ " parsing prolog file:") 
             putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                               (map (\x -> "  " ++ x)) $ 
                               (splitOn "\n" $ show err)
        (_,Left (err :: String)) ->
          do putStrLn ((red colorOpts "Error")++" parsing schema file:") 
             putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
                               (map (\x -> "  " ++ x)) $ 
                               (splitOn "\n" $ show err)
        -- If all files parse sucessfully...
        (Right clauses, Right schema) ->
          case server opts of
        -- Launch server
            True -> do
              case (port opts) of
                Just n -> do
                    putStrLn "Warning: Need to load files here."
                    initBli opts (newServer n)
                Nothing -> putStrLn "Please specify a port number to use the server."
        -- If not configured to start server...
            False -> do
              case goal opts of
                "" -> do
                   -- Print the main banner if options set to verbose.
                   if (verbose opts) then putStrLn $ replBanner versionStr colorOpts else return ()
                   -- Run a bli prolog REPL with the user configuration.
                   putStrLn "Warning: Need to handle getting data from files here."
                   initBli opts repl
                -- If the user supplies a non-empty goal-string, run a single
                -- command rather than starting the REPL.
                input -> do
                    putStrLn "Warning: Need to handle getting data from files here."
                    initBli opts $ processCliInput input