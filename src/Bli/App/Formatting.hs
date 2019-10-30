
-- | Handle formatting the display in the CLI
--

module Bli.App.Formatting where

import Control.Monad.IO.Class
import System.Console.Terminal.Size
import Bli.App.Config.Data

-- | Helper function for printing a response prompt in the Bli monad.
--   Note: This needs to be changed to allow for better screen formatting.
--   I think the general strategey should go something like this:
--      * Ignore newlines
--      * Split the string into words
--      * Create new lines by adding words until this would cause an overflow,
--        in which case we add the word to a new line.
--      * Display the lines with the proper amount of indentation
printResponse :: MonadIO m => String -> m ()
printResponse string = liftIO $ putStrLn $ response_prompt ++ string

