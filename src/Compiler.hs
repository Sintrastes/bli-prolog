
import System.Environment
import Bli.Prolog.Compiler

main = do
  args <- getArgs
  let arg1 = args !! 0
  let arg2 = args !! 1
  compileStatic arg1 arg2
  