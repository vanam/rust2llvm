import           Control.Applicative hiding ((<|>))
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.ParserCombinators.Parsec
--import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token


languageDef =
  emptyDef
    { Token.commentStart = "/*"
    , Token.commentEnd = "*/"
    , Token.commentLine = "//"
    , Token.identStart = letter
    , Token.identLetter = alphaNum
    , Token.reservedNames = []
    , Token.reservedOpNames = ["+", "-", "*", "/"]
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

binary name fun assoc = flip Infix assoc $ reservedOp name *> pure fun
prefix name fun = Prefix $ reservedOp name *> pure fun
postfix name fun = Postfix $ reservedOp name *> pure fun

table = [ [prefix "-" negate, prefix "+" id]
        , [postfix "++" (+ 1)]
        , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft]
        , [binary "+" (+) AssocLeft, binary "-" (-) AssocLeft]
        ]

expr = buildExpressionParser table term
       <?> "expression"

term = parens expr
       <|> integer
           <?> "simple expression"
