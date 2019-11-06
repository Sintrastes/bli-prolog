
module Bli.Prolog.Interp.Data where

import Prelude hiding ((<>))
import Data.Bli.Prolog.Ast
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint ((<>),(<+>))
import qualified Data.Char as C
import GHC.Generics

-- | Datatype representing the solution to a prolog query.
data Solution = 
   Solution [(Variable, Term)]
 | ProcReturn
  deriving (Eq, Read, Generic)


-- | Helper function for showing solutions.
isPlain (c:cs) = C.isLower c && all (\c -> c == '_' || C.isAlphaNum c) cs
isPlain _ = False

instance Show Solution where
  -- This will need to be fixed later to provide
  -- better formatting.
  show (Solution bindings) = PP.render(renderB bindings)
    where
      renderB [] = PP.text "true"
      renderB bindings = PP.braces $ PP.vcat $ map renderBindings bindings

      renderBindings (var, term) = PP.text var <+> PP.equals <+> renderT term

      renderAtom a = PP.text $ show a -- if isPlain a then PP.text a
                            -- else PP.text "'" <> PP.text a <> PP.text "'"

      renderT (Var v) = PP.text v
      renderT (Comp a []) = renderAtom a
      renderT comp@(Comp f args) =
        case listTerm comp of
          Just tt -> PP.brackets $ renderTerms tt
          Nothing -> renderAtom f <> (PP.parens $ renderTerms $ args)

      renderTerms terms = PP.sep $ PP.punctuate PP.comma $ map renderT $ terms

      listTerm (Comp (ListLiteral xs) []) = Just $ map (\x -> Comp x []) xs
      listTerm _                          = Nothing



data SearchTree = Sol Solution
                | Node Goal [SearchTree]
                  deriving (Eq, Show, Read)
