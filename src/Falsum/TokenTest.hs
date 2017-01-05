module Falsum.TokenTest where

-- We could use Template Haskell for automatic deriving of data constructor checking functions
-- <http://stackoverflow.com/a/6089121>, but we would confuse the linter in Atom.
import           Falsum.Lexer

isIntLiteral :: Token -> Bool
isIntLiteral (Literal (IntLit _ _)) = True
isIntLiteral _ = False

isFloatLiteral :: Token -> Bool
isFloatLiteral (Literal (FloatLit _ _)) = True
isFloatLiteral _ = False

isLifeTime :: LifeTime -> Token -> Bool
isLifeTime lt (LifeTime x) = lt == x
isLifeTime _ _ = False

isKeyword :: Keyword -> Token -> Bool
isKeyword k (Keyword x) = k == x
isKeyword _ _ = False

isSymbol :: String -> Token -> Bool
isSymbol s (Symbol x) = s == x
isSymbol _ _ = False

isStructSymbol :: StructureSymbol -> Token -> Bool
isStructSymbol s (StructSym x) = s == x
isStructSymbol _ _ = False

isOperator :: Operator -> Token -> Bool
isOperator o (Operator x) = o == x
isOperator _ _ = False

isAnySymbol :: Token -> Bool
isAnySymbol (Symbol _) = True
isAnySymbol _ = False

isAnyLiteral :: Token -> Bool
isAnyLiteral (Literal _) = True
isAnyLiteral _ = False

isStringLiteral :: Token -> Bool
isStringLiteral (Literal (ByteString _)) = True
isStringLiteral (Literal (UnicodeString _)) = True
isStringLiteral _ = False

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
