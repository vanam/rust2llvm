module Falsum.TokenTest where

-- We could use Template Haskell for automatic deriving of data constructor checking functions
-- <http://stackoverflow.com/a/6089121>, but we would confuse the linter in Atom.
import           Falsum.Lexer

isSymbol :: Symbol -> Token -> Bool
isSymbol s (Symbol x) = s == x
isSymbol _ _ = False

isLiteral :: Literal -> Token -> Bool
isLiteral l (Literal x) = l == x
isLiteral _ _ = False

isLifeTime :: LifeTime -> Token -> Bool
isLifeTime lt (LifeTime x) = lt == x
isLifeTime _ _ = False

isKeyword :: Keyword -> Token -> Bool
isKeyword k (Keyword x) = k == x
isKeyword _ _ = False

isStructSym :: SturctSym -> Token -> Bool
isStructSym s (StructSym x) = s == x
isStructSym _ _ = False

isOperator :: Operator -> Token -> Bool
isOperator o (Operator x) = o == x
isOperator _ _ = False

isIntLit :: IntLit -> Token -> Bool
isIntSuffix i (IntLit x _) = i == x
isIntSuffix _ _ = False

isFloatLit :: IntLit -> Token -> Bool
isIntSuffix i (FloatLit x _) = i == x
isIntSuffix _ _ = False

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
isCoupledDoc (CoupledDoc _ _) = True
isCoupledDoc _ = False

isAnyCoupledAttribute :: Token -> Bool
isCoupledAttribute (CoupledAttribute _ _) = True
isCoupledAttribute _ = False

isAnyIntLit :: Token -> Bool
isIntSuffix (IntLit _ _) = True
isIntSuffix _ = False
