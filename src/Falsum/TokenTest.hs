module Falsum.TokenTest where

-- We could use Template Haskell for automatic deriving of data constructor checking functions
-- <http://stackoverflow.com/a/6089121>, but we would confuse the linter in Atom.
import           Falsum.Lexer

isSymbol :: Token -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False

isLiteral :: Token -> Bool
isLiteral (Literal _) = True
isLiteral _ = False

isLifeTime :: Token -> Bool
isLifeTime (LifeTime _) = True
isLifeTime _ = False

isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _ = False

isStructSym :: Token -> Bool
isStructSym (StructSym _) = True
isStructSym _ = False

isOperator :: Token -> Bool
isOperator (Operator _) = True
isOperator _ = False

isCoupledDoc :: Token -> Bool
isCoupledDoc (CoupledDoc _ _) = True
isCoupledDoc _ = False

isCoupledAttribute :: Token -> Bool
isCoupledAttribute (CoupledAttribute _ _) = True
isCoupledAttribute _ = False
