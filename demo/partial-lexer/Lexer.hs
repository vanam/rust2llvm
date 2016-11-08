module Falsum (tokenizeTest) where

import Control.Applicative ((<*), (*>), (<$>), (<*>))
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (try)
import Data.List (nub, sort)
import Data.Char (isSpace, isAlpha, isDigit, digitToInt, isAscii, isHexDigit, chr, ord)
import Data.Maybe (fromJust)
import ChangeState


type TokenPos = (Token, SourcePos)

withPos :: Parser Token -> Parser TokenPos
withPos p = flip (,) <$> getPosition <*> p

data LifeTime = StaticLife | DynamicLife String
  deriving (Show, Eq)

data IntSuffix = U8 | I8 | U16 | I16 | U32 | I32 | U64 | I64 | ISize | USize
  deriving (Show, Eq)

data FloatSuffix = F32 | F64
  deriving (Show, Eq)

data Literal = IntLit (Maybe IntSuffix) Integer
             | FloatLit (Maybe FloatSuffix) (Either Float Double)
             | ByteString String
             | UnicodeString String
             | ByteChar Char
             | UnicodeChar Char
  deriving (Show, Eq)

data Coupling = Outer | Inner
  deriving (Show, Eq)

data Attribute = Single String
               | KeyValue String String
               | AttributeList String [Attribute]
  deriving (Show, Eq)

data StructureSymbol = Semicolon | Comma | TripleDot | DoubleDot | Dot | LParen | RParen | LBrack | RBrack | LBrace | RBrace | AtSign | Tilde | ModSep | Colon | Dollar | Question | LArrow | RArrow -- | Pound
  deriving (Show, Eq)

data Operator = DoubleEq | FatArrow | EqSign | Neq | Not | Leq | Shl | Shleq | Less | Geq | Shr | Shreq | Greater | Minus | Minuseq | DoubleAnd | And | Andeq | DoubleOr | Or | Oreq | Plus | Pluseq | Star | Stareq | Slash | Slasheq | Caret | Careteq | Percent | Percenteq
  deriving (Show, Eq)

data Token = Symbol String
           | Literal Literal
           | LifeTime LifeTime
           | Keyword Keyword
           | StructSym StructureSymbol
           | Operator Operator
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


isSymbolStart c = '_' == c || isAlpha c
isSymbolLetter c = isSymbolStart c || isDigit c

symbol :: Parser String
symbol = (++)
       <$> (fmap return $ try $ satisfy isSymbolStart)
       <*> (many $ satisfy isSymbolLetter)

simpleSpaces = skipMany $ satisfy isSpace
simpleSpaces1 = skipMany1 $ satisfy isSpace


around a p = a *> p <* a

spacesAround p = simpleSpaces `around` p

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
                      notFollowedBy $ satisfy isSymbolLetter
                      return $ Keyword $ reservedConvert n

attributeContent
  =  try (do ide <- spacesAround symbol
             notFollowedBy $ spacesAround $ oneOf ['=','(']
             return $ Single ide)
  <|> try (do id1 <- symbol
              spacesAround $ char '='
              id2 <- symbol
              return $ KeyValue id1 id2)
  <|> try (do ide <- symbol
              spacesAround $ char '('
              attrs <- attributeContent `sepBy` (spacesAround $ char ',')
              spacesAround $ char ')'
              return $ AttributeList ide attrs)

attribute =
  do start <- (try $ string "#[") <|> (try $ string "#![")
     content <- attributeContent
     char ']'
     return $ CoupledAttribute (if start !! 1 == '!' then Inner else Outer) content


{-lsq, rsq, lbr, rbr :: Parser TokenPos
lpr = withPos $ char '(' >> return LParen
rpr = withPos $ char ')' >> return RParen
lsq = withPos $ char '[' >> return LBrack
rsq = withPos $ char ']' >> return RBrack
lbr = withPos $ char '{' >> return LBrace
rbr = withPos $ char '}' >> return RBrace

brackets = choice [lpr, rpr, lsq, rsq, lbr, rbr]-}

charConvert c = case c of
  'n' -> '\n'
  'r' -> '\r'
  't' -> '\t'
  '\\' -> '\\'
  '0' -> '\0'
  '\'' -> '\''
  '"' -> '"'

stringToInt base = foldl1 (\x y -> x * base + y) . map digitToInt

hexStringToInt = stringToInt 16

upTo n p = choice $ map (try . (flip count p)) $ reverse [1..n]

inChar :: Parser Char
inChar =  try (string "\\u") *> between (char '{') (char '}')
                                        (do digits <- upTo 6 $ satisfy isHexDigit
                                            ordinal <- return $ hexStringToInt digits
                                            if ordinal <= ord maxBound
                                               then return $ chr ordinal
                                               else fail "too big ordinal")
       <|> inByte
       <|> anyChar

inByte :: Parser Char
inByte = try (char '\\' *> char 'x') *> (do digits <- count 2 $ satisfy isHexDigit
                                            return $ chr . hexStringToInt $ digits)
         <|> try (char '\\') *> fmap charConvert (oneOf ['n', 'r', 't', '\\', '0', '\'', '"'])
         <|> satisfy isAscii

character :: Parser Char
character = char '\'' `around` inChar

byte :: Parser Char
byte = try (char 'b') *> char '\'' `around` inByte

indentSkipper = optional $ try $ string "\\\n" *> simpleSpaces

seqLit :: Parser Char -> Parser String
seqLit c = char '"' *> indentSkipper *> manyTill (c <* indentSkipper) (char '"')

stringLit = seqLit inChar
byteStringLit = try (char 'b') *> seqLit inByte

rawSeqLit c = changeState (const ()) (const 0) $
  try (char 'r') *> (do many (char '#' >> modifyState (1+))
                        char '"'
                        hashCount <- getState
                        manyTill c (try $ char '"' >> count hashCount (char '#')))

rawStringLit = rawSeqLit anyChar
rawByteStringLit = try (char 'b') *> rawSeqLit (satisfy isAscii)

stringLiterals = choice . map (withPos . fmap Literal) $
  [ fmap ByteChar $ byte
  , fmap ByteString $ byteStringLit
  , fmap ByteString $ rawByteStringLit
  , fmap UnicodeChar $ character
  , fmap UnicodeString $ stringLit
  , fmap UnicodeString $ rawStringLit
  ]

integerSuffixes = ["u8", "i8", "u16", "i16", "u32", "i32", "u64", "i64", "usize", "isize"]

integerSuffixConvert suf = case suf of
  "u8" -> U8
  "i8" -> I8
  "u16" -> U16
  "i16" -> I16
  "u32" -> U32
  "i32" -> I32
  "u64" -> U64
  "i64" -> I64
  "usize" -> USize
  "isize" -> ISize

floatSuffixes = ["f32", "f64"]

floatSuffixConvert suf = case suf of
  "f32" -> F32
  "f64" -> F64

intLit base digitLetters =
  do digits <- many1 $ (optional (char '_')) `around` digitLetters
     suffix <- (fmap . fmap) integerSuffixConvert $ optionMaybe $ choice $ map (try . string) integerSuffixes
     return $ IntLit suffix $ toInteger $ stringToInt base digits


decLit = intLit 10 digit
hexLit = intLit 16 $ satisfy isHexDigit
octLit = intLit 8 $ satisfy (flip elem ['0'..'7'])
binLit = intLit 2 $ satisfy (flip elem ['0', '1'])

intLits
  =   try (string "0x") *> hexLit
  <|> try (string "0o") *> octLit
  <|> try (string "0b") *> binLit
  <|> decLit

expPart :: Parser Integer
expPart =
 do oneOf ['e', 'E']
    sign <- optionMaybe $ oneOf ['+', '-']
    digits <- many1 $ (optional (char '_')) `around` digit
    return $ (if sign == Just '-' then negate else id) $ toInteger $ stringToInt 10 digits

floatLit =
  do digits <- many1 $ (optional (char '_')) `around` digit
     (do try (char '.' <* lookAhead digit)
         digitsAfter <- many1 $ (optional (char '_')) `around` digit
         ex <- optionMaybe expPart
         suffix <- (fmap . fmap) floatSuffixConvert $ optionMaybe $ choice $ map (try . string) floatSuffixes
         num <- return $ digits ++ ['.'] ++ digitsAfter
         num <- if ex == Nothing then return num else return (num ++ ['e'] ++ show (fromJust ex))
         return $ FloatLit suffix $ if suffix == Just F32
           then Left $ read num
           else Right $ read num
      <|> do try (char '.' <* notFollowedBy (satisfy isSymbolStart))
             return $ FloatLit Nothing (Right $ read digits))
      <|> do ex <- expPart
             suffix <- (fmap . fmap) floatSuffixConvert $ optionMaybe $ choice $ map (try . string) floatSuffixes
             num <- return $ digits ++ ['e'] ++ show ex
             return $ FloatLit suffix $ if suffix == Just F32
               then Left $ read num
               else Right $ read num

lifeTime
  =   do try $ char '\'' *> string "static"
         return StaticLife
  <|> do symb <- try $ char '\'' *> symbol <* notFollowedBy (char '\'')
         return $ DynamicLife symb

syntax1 = [(Semicolon, ";"), (Comma, ","), (TripleDot, "..."), (DoubleDot, ".."), (Dot, "."), (LParen, "("), (RParen, ")"), (LBrace, "{"), (RBrace, "}"), (LBrack, "["), (RBrack, "]"), (AtSign, "@"){-, (Pound, "#")-}, (Tilde, "~"), (ModSep, "::"), (Colon, ":"), (Dollar, "$"), (Question, "?")]

operators1 = [(DoubleEq, "=="), (FatArrow, "=>"), (EqSign, "="), (Neq, "!="), (Not, "!"), (Leq, "<="), (Shleq, "<<="), (Shl, "<<"), (Less, "<"), (Geq, ">="), (Shreq, ">>="), (Shr, ">>"), (Greater, ">")]

syntax2 = [(LArrow, "<-"), (RArrow, "->")]

operators2 = [(Minuseq, "-="), (Minus, "-"), (DoubleAnd, "&&"), (And, "&"), (DoubleOr, "||"), (Oreq, "|="), (Or, "|"), (Pluseq, "+="), (Plus, "+"), (Stareq, "*="), (Star, "*"), (Slasheq, "/="), (Slash, "/"), (Careteq, "^="), (Caret, "^"), (Percenteq, "%="), (Percent, "%")]

parserize (val, str) = try $ string str *> return val
combineTrivial constructor trivials = choice $ map (withPos . fmap constructor . parserize) trivials

arbitraryToken :: Parser TokenPos
arbitraryToken = choice
    [ try reservedName
    , withPos $ fmap LifeTime $ lifeTime
    , stringLiterals
    , withPos $ fmap Symbol $ symbol
    , withPos $ docComment
    , withPos $ attribute
    , combineTrivial StructSym syntax1
    , combineTrivial Operator operators1
    , combineTrivial StructSym syntax2
    , combineTrivial Operator operators2
    , withPos $ fmap Literal $ try floatLit
    , withPos $ fmap Literal $ intLits
    ]

tokenizer :: Parser [TokenPos]
tokenizer = optional bom *> whiteSpaces
          *> optional shebang *> whiteSpaces
          *> many (arbitraryToken <* whiteSpaces)

tokenizeTest :: String -> IO ()
tokenizeTest = parseTest tokenizer