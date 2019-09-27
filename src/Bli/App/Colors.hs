
-- | Utility functions for printing text with a specific color
--   for the bli-prolog cli. The boolean argument of each
--   of the functions in this module controls whether or not 
--   the color has been configured to display.

module Bli.App.Colors where

red :: Bool -> String -> String
red True s = "\27[31m" ++ s ++ "\27[37m"
red False s = s

blue :: Bool -> String -> String
blue True s = "\27[36m" ++ s ++ "\27[37m"
blue False s = s

yellow :: Bool -> String -> String
yellow True s = "\27[33m" ++ s ++ "\27[37m"
yellow False s = s

green :: Bool -> String -> String
green True s = "\27[32m" ++ s ++ "\27[37m"
green False s = s