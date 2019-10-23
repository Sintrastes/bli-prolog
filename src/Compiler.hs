
import System.Environment
import Bli.Prolog.Compiler

main = do
  args <- getArgs
  case length args of
    0 -> do 
      putStrLn "Error parsing command line arguments. Must be called with one of the following formats:"
      putStrLn "  blic [source_file] [output_file]"
      putStrLn "  blic [source_file]"
      putStrLn "In case of the second format, the executable will be installed to the current directory,"
      putStrLn "with the same name as the source file, but without a .bpl extension."
    1 -> do
      let arg1 = args !!0
      compileStatic arg1 ""
    2 -> do
      let arg1 = args !! 0
      let arg2 = args !! 1
      compileStatic arg1 arg2
  