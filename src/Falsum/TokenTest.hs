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

isColon :: Token -> Bool
isColon (StructSym Colon) = True
isColon _ = False

isSemicolon :: Token -> Bool
isSemicolon (StructSym Semicolon) = True
isSemicolon _ = False

isLet :: Token -> Bool
isLet (Keyword Let) = True
isLet _ = False

isConst :: Token -> Bool
isConst (Keyword Const) = True
isConst _ = False

isFn :: Token -> Bool
isFn (Keyword Fn) = True
isFn _ = False

isMut :: Token -> Bool
isMut (Keyword Mut) = True
isMut _ = False

isFor :: Token -> Bool
isFor (Keyword For) = True
isFor _ = False

isLoop :: Token -> Bool
isLoop (Keyword Loop) = True
isLoop _ = False

isIf :: Token -> Bool
isIf (Keyword If) = True
isIf _ = False

isElse :: Token -> Bool
isElse (Keyword Else) = True
isElse _ = False

isIntSuffix :: Token -> Bool
isIntSuffix (IntLit IntSuffix _) = True
isIntSuffix _ = False

isFloatSuffix :: Token -> Bool
isFloatSuffix (IntLit FloatSuffix _) = True
isFloatSuffix _ = False

isEqualSign :: Token -> Bool
isEqualSign (Operator EqSign) = True
isEqualSign _ = False
