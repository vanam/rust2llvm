--import           Control.Applicative hiding ((<|>))
import           Data.Functor.Identity
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec
--import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

languageDef :: GenLanguageDef String () Identity
languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames = []
    , Token.reservedOpNames = ["++", "+", "-", "*", "/"]
    }

lexer :: Token.GenTokenParser String () Identity
lexer = Token.makeTokenParser languageDef

-- identifier = Token.identifier lexer reserved = Token.reserved lexer
reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

-- semi = Token.semi lexer whiteSpace = Token.whiteSpace lexer
{-data Term a = Term a
            | NestedExpr a
            deriving (Show)-}
data Expr a = Term a
            | NestedExpr (Expr a)
            | Unary String (Expr a)
            | Binary String (Expr a) (Expr a)
  deriving (Show)

binary :: String -> (String -> a -> a -> a) -> Assoc -> Operator String () Identity a
binary name con assoc = flip Infix assoc $ reservedOp name *> pure (con name)

prefix :: String -> (String -> a -> a) -> Operator String () Identity a
prefix name con = Prefix $ reservedOp name *> pure (con name)

postfix :: String -> (String -> a -> a) -> Operator String () Identity a
postfix name con = Postfix $ reservedOp name *> pure (con name)

table :: [[Operator String () Identity (Expr a)]]
table = [ [prefix "-" Unary, (Prefix $ reservedOp "+" *> pure id)]
        , [postfix "++" Unary]
        , [binary "*" Binary AssocLeft, binary "/" Binary AssocLeft]
        , [binary "+" Binary AssocLeft, binary "-" Binary AssocLeft]
        ]

expr :: Parser (Expr Integer)
expr = buildExpressionParser table term
       <?> "expression"

term :: Parser (Expr Integer)
term = fmap NestedExpr (parens expr)
       <|> fmap Term integer
           <?> "simple expression"

main :: IO ()
main = do
  input <- getLine
  parseTest expr input
