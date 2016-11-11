module Falsum.Lexer (tokenize, tokenizeTest) where

import           ChangeState
import           Control.Applicative           ((*>), (<$>), (<*), (<*>))
import           Data.Char                     (chr, digitToInt, isAlpha,
                                                isAscii, isDigit, isHexDigit,
                                                isSpace, ord)
import           Data.List                     (nub, sort)
import           Data.Maybe                    (fromJust)
import           Data.Tuple                    (swap)
import           Text.Parsec.Prim              hiding (runParser, try)
import           Text.ParserCombinators.Parsec

type TokenPos = (Token, SourcePos)

withPos :: Parser Token -> Parser TokenPos
withPos p = flip (,) <$> getPosition <*> p

data LifeTime = StaticLife
              | DynamicLife String
  deriving (Show, Eq)

data IntSuffix = U8
               | I8
               | U16
               | I16
               | U32
               | I32
               | U64
               | I64
               | USize
               | ISize
  deriving (Show, Eq)

integerSuffixTable = [ (U8, "u8")
                     , (I8, "i8")
                     , (U16, "u16")
                     , (I16, "i16")
                     , (U32, "u32")
                     , (I32, "i32")
                     , (U64, "u64")
                     , (I64, "i64")
                     , (USize, "usize")
                     , (ISize, "isize")
                     ]

data FloatSuffix = F32
                 | F64
  deriving (Show, Eq)

floatSuffixeTable = [(F32, "f32"), (F64, "f64")]

data Literal = IntLit (Maybe IntSuffix) Integer
             | FloatLit (Maybe FloatSuffix) (Either Float Double)
             | ByteString String
             | UnicodeString String
             | ByteChar Char
             | UnicodeChar Char
  deriving (Show, Eq)

data Coupling = Outer
              | Inner
  deriving (Show, Eq)

data Attribute = Single String
               | KeyValue String Literal
               | AttributeList String [Attribute]
  deriving (Show, Eq)

data StructureSymbol = Semicolon
                     | Comma
                     | TripleDot
                     | DoubleDot
                     | Dot
                     | LParen
                     | RParen
                     | LBrack
                     | RBrack
                     | LBrace
                     | RBrace
                     | AtSign
                     | Tilde
                     | ModSep
                     | Colon
                     | Dollar
                     | Question
                     | LArrow
                     | RArrow -- | Pound
  deriving (Show, Eq)

structureSymbolTable = [ (Semicolon, ";")
                       , (Comma, ",")
                       , (TripleDot, "...")
                       , (DoubleDot, "..")
                       , (Dot, ".")
                       , (LParen, "(")
                       , (RParen, ")")
                       , (LBrace, "{")
                       , (RBrace, "}")
                       , (LBrack, "[")
                       , (RBrack, "]")
                       , (AtSign, "@"){-, (Pound, "#")-}
                       , (Tilde, "~")
                       , (ModSep, "::")
                       , (Colon, ":")
                       , (Dollar, "$")
                       , (Question, "?")
                       , (LArrow, "<-")
                       , (RArrow, "->")
                       ]

data Operator = DoubleEq
              | FatArrow
              | EqSign
              | Neq
              | Not
              | Leq
              | Shl
              | Shleq
              | Less
              | Geq
              | Shr
              | Shreq
              | Greater
              | Minus
              | Minuseq
              | DoubleAnd
              | And
              | Andeq
              | DoubleOr
              | Or
              | Oreq
              | Plus
              | Pluseq
              | Star
              | Stareq
              | Slash
              | Slasheq
              | Caret
              | Careteq
              | Percent
              | Percenteq
  deriving (Show, Eq)

operatorTable = [ (DoubleEq, "==")
                , (FatArrow, "=>")
                , (EqSign, "=")
                , (Neq, "!=")
                , (Not, "!")
                , (Leq, "<=")
                , (Shleq, "<<=")
                , (Shl, "<<")
                , (Less, "<")
                , (Geq, ">=")
                , (Shreq, ">>=")
                , (Shr, ">>")
                , (Greater, ">")
                , (Minuseq, "-=")
                , (Minus, "-")
                , (DoubleAnd, "&&")
                , (And, "&")
                , (DoubleOr, "||")
                , (Oreq, "|=")
                , (Or, "|")
                , (Pluseq, "+=")
                , (Plus, "+")
                , (Stareq, "*=")
                , (Star, "*")
                , (Slasheq, "/=")
                , (Slash, "/")
                , (Careteq, "^=")
                , (Caret, "^")
                , (Percenteq, "%=")
                , (Percent, "%")
                ]

data Token = Symbol String
           | Literal Literal
           | LifeTime LifeTime
           | Keyword Keyword
           | StructSym StructureSymbol
           | Operator Operator
           | CoupledDoc Coupling String
           | CoupledAttribute Coupling Attribute
  deriving (Show, Eq)

data Keyword = Underscore
             | As
             | Box
             | Break
             | Const
             | Continue
             | Crate
             | Else
             | Enum
             | Extern
             | FalseLit
             | Fn
             | For
             | If
             | Impl
             | In
             | Let
             | Loop
             | Match
             | Mod
             | Move
             | Mut
             | Priv
             | Proc
             | Pub
             | Ref
             | Return
             | Self
             | Static
             | Struct
             | Trait
             | TrueLit
             | Type
             | TypeOf
             | Unsafe
             | Use
             | Where
             | While
  deriving (Show, Eq)

keywordTable = [ (Underscore, "_")
               , (As, "as")
               , (Box, "break")
               , (Const, "const")
               , (Continue, "continue")
               , (Crate, "crate")
               , (Else, "else")
               , (Enum, "enum")
               , (Extern, "extern")
               , (FalseLit, "false")
               , (Fn, "fn")
               , (For, "for")
               , (If, "if")
               , (Impl, "impl")
               , (In, "in")
               , (Let, "let")
               , (Loop, "loop")
               , (Match, "match")
               , (Mod, "mod")
               , (Move, "move")
               , (Mut, "mut")
               , (Priv, "priv")
               , (Proc, "proc")
               , (Return, "return")
               , (Self, "self")
               , (Static, "static")
               , (Struct, "struct")
               , (Trait, "trait")
               , (TrueLit, "true")
               , (Type, "type")
               , (TypeOf, "typeof")
               , (Use, "use")
               , (Where, "where")
               , (While, "while")
               ]

forwardLookup table = fromJust . flip lookup table

backwardLookup table = fromJust . flip lookup (map swap table)

bom = string "\xef\xbb\xbf"

shebang =
  do
    try $ string "#!"
    notFollowedBy $ string "["
    pos <- getPosition
    if sourceLine pos /= 1
      then fail "unexpected shebang"
      else return ()
    skipMany $ satisfy (/= '\n')
    return ()

isSymbolStart c = '_' == c || isAlpha c

isSymbolLetter c = isSymbolStart c || isDigit c

symbol :: Parser String
symbol = (++) <$> (fmap return $ try $ satisfy isSymbolStart) <*> (many $ satisfy isSymbolLetter)

simpleSpaces = skipMany $ satisfy isSpace

simpleSpaces1 = skipMany1 $ satisfy isSpace

around a p = a *> p <* a

spacesAround p = simpleSpaces `around` p

lineDocStart = (try $ string "///") <|> (try $ string "//!")

oneLineComment =
  do
    notFollowedBy lineDocStart
    try $ string "//"
    skipMany $ satisfy (/= '\n')
    return ()

inComment = do
              try $ string "*/"
            <|> do
              start <- try $ string "/*"
              nested <- inComment
              content <- inComment
              return $ start ++ nested ++ content
            <|> do
              content <- many1 $ noneOf startEnd
              next <- inComment
              return $ content ++ next
            <|> do
                  specialChar <- oneOf startEnd
                  next <- inComment
                  return $ [specialChar] ++ next
                <?> "end of comment"
  where
    startEnd = nub $ "*/" ++ "/*"

multiDocStart =
  do
    notFollowedBy (string "/**/")
    (try $ string "/**") <|> (try $ string "/*!")

multiLineComment =
  do
    notFollowedBy multiDocStart
    try $ string "/*"
    inComment
    return ()

whiteSpaces = skipMany (simpleSpaces1 <|> oneLineComment <|> multiLineComment <?> "")

oneLineDocComment =
  do
    start <- lineDocStart
    content <- many $ satisfy (/= '\n')
    return $ CoupledDoc
               (if start !! 2 == '!'
                  then Inner
                  else Outer)
               content

multiLineDocComment =
  do
    start <- multiDocStart
    content <- inComment
    return $ CoupledDoc
               (if start !! 2 == '!'
                  then Inner
                  else Outer) $ init . init $ content

docComment = oneLineDocComment <|> multiLineDocComment <?> ""

reservedName = choice . map keywordParser $ sort $ map snd keywordTable
  where
    keywordParser name = withPos $ do
      n <- try $ string name
      notFollowedBy $ satisfy isSymbolLetter
      return $ Keyword $ backwardLookup keywordTable n

attributeContent = try
                     (do
                        ide <- spacesAround symbol
                        notFollowedBy $ spacesAround $ oneOf ['=', '(']
                        return $ Single ide)
                   <|> try
                         (do
                            id1 <- symbol
                            spacesAround $ char '='
                            id2 <- choice
                                     [ fmap ByteChar $ byte
                                     , fmap ByteString $ byteStringLit
                                     , fmap ByteString $ rawByteStringLit
                                     , fmap UnicodeChar $ character
                                     , fmap UnicodeString $ stringLit
                                     , fmap UnicodeString $ rawStringLit
                                     , try floatLit
                                     , intLits
                                     ]
                            return $ KeyValue id1 id2)
                   <|> try
                         (do
                            ide <- symbol
                            spacesAround $ char '('
                            attrs <- attributeContent `sepBy` (spacesAround $ char ',')
                            spacesAround $ char ')'
                            return $ AttributeList ide attrs)

attribute =
  do
    start <- (try $ string "#[") <|> (try $ string "#![")
    content <- attributeContent
    char ']'
    return $ CoupledAttribute
               (if start !! 1 == '!'
                  then Inner
                  else Outer)
               content

charConvert c =
  case c of
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    '\\' -> '\\'
    '0'  -> '\0'
    '\'' -> '\''
    '"'  -> '"'

stringToInt base = foldl1 (\x y -> x * base + y) . map digitToInt

hexStringToInt = stringToInt 16

upTo n p = choice $ map (try . (flip count p)) $ reverse [1 .. n]

inChar :: Parser Char
inChar = try (string "\\u") *> between (char '{') (char '}')
                                 (do
                                    digits <- upTo 6 $ satisfy isHexDigit
                                    ordinal <- return $ hexStringToInt digits
                                    if ordinal <= ord maxBound
                                      then return $ chr ordinal
                                      else fail "too big ordinal")
         <|> inByte
         <|> anyChar

inByte :: Parser Char
inByte = try (char '\\' *> char 'x') *> (do
                                           digits <- count 2 $ satisfy isHexDigit
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
  try (char 'r') *> (do
                       many (char '#' >> modifyState (1 +))
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

intLit base digitLetters =
  do
    digits <- many1 $ (optional (char '_')) `around` digitLetters
    suffix <- (fmap . fmap) (backwardLookup integerSuffixTable) $
                optionMaybe $ choice $ map (try . string . snd) integerSuffixTable
    return $ IntLit suffix $ toInteger $ stringToInt base digits

decLit = intLit 10 digit

hexLit = intLit 16 $ satisfy isHexDigit

octLit = intLit 8 $ satisfy (`elem` ['0' .. '7'])

binLit = intLit 2 $ satisfy (`elem` ['0', '1'])

intLits = try (string "0x") *> hexLit
          <|> try (string "0o") *> octLit
          <|> try (string "0b") *> binLit
          <|> decLit

expPart :: Parser Integer
expPart =
  do
    oneOf ['e', 'E']
    sign <- optionMaybe $ oneOf ['+', '-']
    digits <- many1 $ (optional (char '_')) `around` digit
    return $ (if sign == Just '-'
                then negate
                else id) $ toInteger $ stringToInt 10 digits

floatLit =
  do
    digits <- many1 $ (optional (char '_')) `around` digit
    (do
       try (char '.' <* lookAhead digit)
       digitsAfter <- many1 $ (optional (char '_')) `around` digit
       ex <- optionMaybe expPart
       suffix <- (fmap . fmap) (backwardLookup floatSuffixeTable) $
                   optionMaybe $ choice $ map (try . string . snd) floatSuffixeTable
       num <- return $ digits ++ ['.'] ++ digitsAfter
       num <- if ex == Nothing
                then return num
                else return (num ++ ['e'] ++ show (fromJust ex))
       return $ FloatLit suffix $ if suffix == Just F32
                                    then Left $ read num
                                    else Right $ read num
     <|> do
       try (char '.' <* notFollowedBy (satisfy isSymbolStart))
       return $ FloatLit Nothing (Right $ read digits))
    <|> do
      ex <- expPart
      suffix <- (fmap . fmap) (backwardLookup floatSuffixeTable) $
                  optionMaybe $ choice $ map (try . string . snd) floatSuffixeTable
      num <- return $ digits ++ ['e'] ++ show ex
      return $ FloatLit suffix $ if suffix == Just F32
                                   then Left $ read num
                                   else Right $ read num

lifeTime = do
             try $ char '\'' *> string "static"
             return StaticLife
           <|> do
             symb <- try $ char '\'' *> symbol <* notFollowedBy (char '\'')
             return $ DynamicLife symb

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
                   , combineTrivial StructSym structureSymbolTable
                   , combineTrivial Operator operatorTable
                   , withPos $ fmap Literal $ try floatLit
                   , withPos $ fmap Literal $ intLits
                   ]

tokenizer :: Parser [TokenPos]
tokenizer = optional bom *> whiteSpaces
            *> optional shebang *> whiteSpaces
            *> many (arbitraryToken <* whiteSpaces)

tokenizeTest :: String -> IO ()
tokenizeTest = parseTest tokenizer

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokenizer ()
