module Falsum (tokenizeTest) where

import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Text.ParserCombinators.Parsec
import Data.List (nub, sort)
import Data.Char (isAlpha, toLower, toUpper, isSpace, digitToInt)


type TokenPos = (Token, SourcePos)

withPos :: Parser Token -> Parser TokenPos
withPos p = flip (,) <$> getPosition <*> p

data Literal = Integer Int
             | String String
             | Bool Bool
             | Char Char
  deriving (Show, Eq)

data Coupling = Outer | Inner
  deriving (Show, Eq)

data Attribute = Single String
               | KeyValue String String
               | AttributeList String [Attribute]
  deriving (Show, Eq)

data Token = Ide String
           | Literal Literal
           | Keyword Keyword
           | LParen | RParen
           | LBrack | RBrack
           | LBrace | RBrace
           | CoupledDoc Coupling String
           | CoupledAttribute Coupling Attribute
    deriving (Show, Eq)

data Keyword = Underscore | As | Box | Break | Const | Continue | Crate | Else | Enum | Extern | False | Fn | For | If | Impl | In | Let | Loop | Match | Mod | Move | Mut | Priv | Proc | Pub | Ref | Return | Self | Static | Struct | Trait | True | Type | TypeOf | Unsafe | Use | Where | While deriving (Show, Eq)
             
reservedNameList = ["_", "as", "box", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "priv", "proc", "pub", "ref", "return", "self", "static", "struct", "trait", "true", "type", "typeof", "unsafe", "use", "where", "while"]

bom = string "\xef\xbb\xbf"

shebang =
  do try $ string "#!"
     notFollowedBy $ string "["
     pos <- getPosition
     if sourceLine pos /= 1 then fail "unexpected shebang" else return ()
     skipMany $ satisfy (/= '\n')
     return ()

identifierStart = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
identifierLetter = identifierStart ++ ['0'..'9']

simpleIdentifier =
  do fc  <- try $ oneOf identifierStart
     r   <- many $ oneOf identifierLetter
     return $ [fc] ++ r

simpleSpaces = skipMany (satisfy isSpace)
simpleSpaces1 = skipMany1 (satisfy isSpace)

spacesAround p = simpleSpaces *> p <* simpleSpaces

lineDocStart = (try $ string "///") <|> (try $ string "//!")

oneLineComment =
  do notFollowedBy lineDocStart
     try $ string "//"
     skipMany $ satisfy (/= '\n')
     return ()

inComment
  =   do try $ string "*/"
  <|> do start <- try $ string "/*"
         nested <- inComment
         content <- inComment
         return $ start ++ nested ++ content
  <|> do content <- many1 $ noneOf startEnd
         next <- inComment
         return $ content ++ next
  <|> do specialChar <- oneOf startEnd
         next <- inComment
         return $ [specialChar] ++ next
  <?> "end of comment"
  where
    startEnd = nub $ "*/" ++ "/*"


multiDocStart =
  do notFollowedBy (string "/**/")
     (try $ string "/**") <|> (try $ string "/*!")

multiLineComment =
  do notFollowedBy multiDocStart
     try $ string "/*"
     inComment
     return ()

whiteSpaces = skipMany (simpleSpaces1 <|> oneLineComment <|> multiLineComment <?> "")

oneLineDocComment = 
  do start <- lineDocStart
     content <- many $ satisfy (/= '\n')
     return $ CoupledDoc (if start !! 2 == '!' then Inner else Outer) content

multiLineDocComment =
  do start <- multiDocStart
     content <- inComment
     return $ CoupledDoc (if start !! 2 == '!' then Inner else Outer) $ init . init $ content

docComment = oneLineDocComment <|> multiLineDocComment <?> ""
--docComments = many (lexeme oneLineDocComment <|> lexeme multiLineDocComment <?> "")

{-
symbol name = lexeme $ string name

lexeme p =
  do x <- p
     whiteSpaces
     return x
-}

reservedConvert :: String -> Keyword
reservedConvert string = case string of
  "_" -> Underscore
  "as" -> As
  "box" -> Box
  "break" -> Break
  "const" -> Const
  "continue" -> Continue
  "crate" -> Crate
  "else" -> Else
  "enum" -> Enum
  "extern" -> Extern
  "false" -> Falsum.False
  "fn" -> Fn
  "for" -> For
  "if" -> If
  "impl" -> Impl
  "in" -> In
  "let" -> Let
  "loop" -> Loop
  "match" -> Match
  "mod" -> Mod
  "move" -> Move
  "mut" -> Mut
  "priv" -> Priv
  "proc" -> Proc
  "pub" -> Pub
  "ref" -> Ref
  "return" -> Return
  "self" -> Self
  "static" -> Static
  "struct" -> Struct
  "trait" -> Trait
  "true" -> Falsum.True
  "type" -> Type
  "typeof" -> TypeOf
  "unsafe" -> Unsafe
  "use" -> Use
  "where" -> Where
  "while" -> While

reservedName = choice . map p $ sort reservedNameList
  where p name = withPos $ 
                   do n <- try $ string name
                      notFollowedBy $ oneOf identifierLetter
                      return $ Keyword $ reservedConvert n

attributeContent
  =  try (do ide <- spacesAround simpleIdentifier
             notFollowedBy $ spacesAround $ oneOf ['=','(']
             return $ Single ide)
  <|> try (do id1 <- simpleIdentifier
              spacesAround $ char '='
              id2 <- simpleIdentifier
              return $ KeyValue id1 id2)
  <|> try (do ide <- simpleIdentifier
              spacesAround $ char '('
              attrs <- attributeContent `sepBy` (spacesAround $ char ',')
              spacesAround $ char ')'
              return $ AttributeList ide attrs)

attribute =
  do start <- (try $ string "#[") <|> (try $ string "#![")
     content <- attributeContent
     char ']'
     return $ CoupledAttribute (if start !! 1 == '!' then Inner else Outer) content


lsq, rsq, lbr, rbr :: Parser TokenPos
lpr = withPos $ char '(' >> return LParen
rpr = withPos $ char ')' >> return RParen
lsq = withPos $ char '[' >> return LBrack
rsq = withPos $ char ']' >> return RBrack
lbr = withPos $ char '{' >> return LBrace
rbr = withPos $ char '}' >> return RBrace

brackets = choice [lpr, rpr, lsq, rsq, lbr, rbr]

arbitraryToken :: Parser TokenPos
arbitraryToken = choice
    [ try reservedName
    , withPos $ fmap Ide $ simpleIdentifier
    , withPos $ docComment
    , withPos $ attribute
    , brackets
    ]

tokenizer :: Parser [TokenPos]
tokenizer = optional bom *> whiteSpaces
          *> optional shebang *> whiteSpaces
          *> many (arbitraryToken <* whiteSpaces)

{-tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokenizer ()-}

tokenizeTest :: String -> IO ()
tokenizeTest = parseTest tokenizer