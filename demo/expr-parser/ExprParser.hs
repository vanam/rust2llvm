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
binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name fun assoc = flip Infix assoc $ reservedOp name *> pure fun

prefix :: String -> (a -> a) -> Operator String () Identity a
prefix name fun = Prefix $ reservedOp name *> pure fun

postfix :: String -> (a -> a) -> Operator String () Identity a
postfix name fun = Postfix $ reservedOp name *> pure fun

table :: [[Operator String () Identity Integer]]
table = [ [prefix "-" negate, prefix "+" id]
        , [postfix "++" (+ 1)]
        , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft]
        , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
        ]

expr :: Parser Integer
expr = buildExpressionParser table term
       <?> "expression"

term :: Parser Integer
term = parens expr
       <|> integer
           <?> "simple expression"

main :: IO ()
main = do
  input <- getLine
  parseTest expr input
