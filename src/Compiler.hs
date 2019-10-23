
import System.Environment
import Bli.Prolog.Compiler
import Bli.App.Config.Version
import Bli.App.Config.Executables

main = do
  args <- getArgs
  case length args of
    0 -> do 
      let name = $(getCompilerNameFromCabal)
      putStrLn "Error parsing command line arguments. Must be called with one of the following formats:"
      putStrLn $ "  "++name++" [source_file] [output_file]"
      putStrLn $ "  "++name++" [source_file]"
      putStrLn "In case of the second format, the executable will be installed to the current directory,"
      putStrLn "with the same name as the source file, but without a .bpl extension."
    1 -> do
      let arg1 = args !! 0
      case arg1 of
        "--help"  -> printHelpScreen
        "-h"      -> printHelpScreen
        otherwise -> compileStatic arg1 ""
    2 -> do
      let arg1 = args !! 0
      let arg2 = args !! 1
      putStrLn arg1
      putStrLn arg2
      compileStatic arg1 arg2
  
printHelpScreen = putStrLn $ foldr1 (\x -> \y -> x ++ "\n" ++ y) $
  ["bli-prolog compiler v"++version++", (C) Nathan Bedell 2019.",
   "   Visit https://github.com/Sintrastes/bli-prolog for more documentation on Bedelibry Prolog.",
   "",
   name++" [source_file]                   Compiles the specified source file to an executable",
   "                                     in the same directory, with the same name as the source",
   "                                     file, but without extension.",
   name++" [source_file] [output_file]     Compiles the specified source file to an exectuable",
   "                                     [output_file].",
   "",
   "Flags:",
   "  --help      Displays this help screen.",
   "  --dyn       Compiles the file, dynamically linknig any dependent modules.",
   "  --bytecode  Compiles to Bedelibry Prolog bytecode instead of linking into",
   "              a static executable."
  ]
 where Just version = $(getVersionFromCabal)
       name         = $(getCompilerNameFromCabal) 