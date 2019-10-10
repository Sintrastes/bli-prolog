
module Prolog.Interp.Data where

import Prelude hiding ((<>))
import Data.Prolog.Ast
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>),(<+>))
import qualified Data.Char as C

-- | Datatype representing the solution to a prolog query.
newtype Solution = Solution [(Variable, Term)]
                 deriving (Eq, Read)


-- | Helper function for showing solutions.
isPlain (c:cs) = C.isLower c && all (\c -> c == '_' || C.isAlphaNum c) cs
isPlain _ = False

instance Show Solution where
  show (Solution bindings) = PP.render(renderB bindings)
    where
      renderB [] = PP.text "true"
      renderB bindings = PP.braces $ PP.vcat $ map renderBindings bindings

      renderBindings (var, term) = PP.text var <+> PP.equals <+> renderT term

      renderAtom a = if isPlain a then PP.text a
                     else PP.text "'" <> PP.text a <> PP.text "'"

      renderT (Var v) = PP.text v
      renderT (Comp a []) = renderAtom a
      renderT comp@(Comp f args) =
        case listTerm comp of
          Just tt -> PP.brackets $ renderTerms tt
          Nothing -> renderAtom f <> (PP.parens $ renderTerms args)

      renderTerms terms = PP.sep $ PP.punctuate PP.comma $ map renderT terms

      listTerm (Comp "[]" [])    = return []
      listTerm (Comp "." [h, t]) = do tt <- listTerm t
                                      return $ h:tt
      listTerm _                 = Nothing

data SearchTree = Sol Solution
                | Node Goal [SearchTree]
                  deriving (Eq, Show, Read)
