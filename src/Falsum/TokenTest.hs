module Falsum.TokenTest where

-- We could use Template Haskell for automatic deriving of data constructor checking functions
-- <http://stackoverflow.com/a/6089121>, but we would confuse the linter in Atom.
import           Falsum.Lexer

isLiteral :: Literal -> Token -> Bool
isLiteral l (Literal x) = l == x
isLiteral _ _ = False

isLifeTime :: LifeTime -> Token -> Bool
isLifeTime lt (LifeTime x) = lt == x
isLifeTime _ _ = False

isKeyword :: Keyword -> Token -> Bool
isKeyword k (Keyword x) = k == x
isKeyword _ _ = False

isStructSym :: StructureSymbol -> Token -> Bool
isStructSym s (StructSym x) = s == x
isStructSym _ _ = False

isOperator :: Operator -> Token -> Bool
isOperator o (Operator x) = o == x
isOperator _ _ = False

isAnySymbol :: Token -> Bool
isAnySymbol (Symbol _) = True
isAnySymbol _ = False

isAnyLiteral :: Token -> Bool
isAnyLiteral (Literal _) = True
isAnyLiteral _ = False

isAnyLifeTime :: Token -> Bool
isAnyLifeTime (LifeTime _) = True
isAnyLifeTime _ = False

isAnyKeyword :: Token -> Bool
isAnyKeyword (Keyword _) = True
isAnyKeyword _ = False

isAnyStructSym :: Token -> Bool
isAnyStructSym (StructSym _) = True
isAnyStructSym _ = False

isAnyOperator :: Token -> Bool
isAnyOperator (Operator _) = True
isAnyOperator _ = False

isAnyCoupledDoc :: Token -> Bool
isAnyCoupledDoc (CoupledDoc _ _) = True
isAnyCoupledDoc _ = False

isAnyCoupledAttribute :: Token -> Bool
isAnyCoupledAttribute (CoupledAttribute _ _) = True
isAnyCoupledAttribute _ = False
