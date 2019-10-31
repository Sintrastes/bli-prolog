
-- | Parsers for procedure declarations.

module Bli.Prolog.Parser.Procs where

import Data.BliParser
import Bli.Prolog.Parser.Common
import Bli.Prolog.Parser.Util
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

schemaProcP :: BliParser ()
schemaProcP = do
  symb "proc"
  id <-identifierP
  csymb ':'
  args <- identifierP `sepBy1` (csymb ',')
  csymb '.'
  return ()

schemaEmptyProcP :: BliParser ()
schemaEmptyProcP = do
  symb "proc"
  id <- identifierP
  csymb '.'
  return ()

schemaExternProcP :: BliParser ()
schemaExternProcP = do
  symb "extern"
  schemaProcP

schemaEmptyExternProcP :: BliParser ()
schemaEmptyExternProcP = do
  symb "extern"
  schemaEmptyProcP
