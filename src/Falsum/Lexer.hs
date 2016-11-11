module Falsum.Lexer (tokenize, tokenizeTest) where

import           ChangeState
import           Control.Applicative           ((*>), (<$>), (<*), (<*>))
import           Data.Char                     (chr, digitToInt, isAlpha,
                                                isAscii, isDigit, isHexDigit,
                                                isSpace, ord)
import           Data.Functor.Identity         (Identity)
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

integerSuffixTable :: [(IntSuffix, String)]
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

floatSuffixTable :: [(FloatSuffix, String)]
floatSuffixTable = [(F32, "f32"), (F64, "f64")]

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

structureSymbolTable :: [(StructureSymbol, String)]
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

operatorTable :: [(Operator, String)]
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

keywordTable :: [(Keyword, String)]
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

forwardLookup :: Eq a => [(a, b)] -> a -> b
forwardLookup table = fromJust . flip lookup table

backwardLookup :: Eq b => [(a, b)] -> b -> a
backwardLookup table = fromJust . flip lookup (map swap table)

bom :: Parser String
bom = string "\xef\xbb\xbf"

shebang :: Parser ()
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

isSymbolStart :: Char -> Bool
isSymbolStart c = '_' == c || isAlpha c

isSymbolLetter :: Char -> Bool
isSymbolLetter c = isSymbolStart c || isDigit c

symbol :: Parser String
symbol = (++) <$> (fmap return $ try $ satisfy isSymbolStart) <*> (many $ satisfy isSymbolLetter)

simpleSpaces :: Parser ()
simpleSpaces = skipMany $ satisfy isSpace

simpleSpaces1 :: Parser ()
simpleSpaces1 = skipMany1 $ satisfy isSpace

around :: Parser a -> Parser b -> Parser b
around a p = a *> p <* a

spacesAround :: Parser a -> Parser a
spacesAround p = simpleSpaces `around` p

lineDocStart :: Parser String
lineDocStart = (try $ string "///") <|> (try $ string "//!")

oneLineComment :: Parser ()
oneLineComment =
  do
    notFollowedBy lineDocStart
    try $ string "//"
    skipMany $ satisfy (/= '\n')
    return ()

inComment :: Parser String
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

multiDocStart :: Parser String
multiDocStart =
  do
    notFollowedBy (string "/**/")
    (try $ string "/**") <|> (try $ string "/*!")

multiLineComment :: Parser ()
multiLineComment =
  do
    notFollowedBy multiDocStart
    try $ string "/*"
    inComment
    return ()

whiteSpaces :: Parser ()
whiteSpaces = skipMany (simpleSpaces1 <|> oneLineComment <|> multiLineComment <?> "")

oneLineDocComment :: Parser Token
oneLineDocComment =
  do
    start <- lineDocStart
    content <- many $ satisfy (/= '\n')
    return $ CoupledDoc
               (if start !! 2 == '!'
                  then Inner
                  else Outer)
               content

multiLineDocComment :: Parser Token
multiLineDocComment =
  do
    start <- multiDocStart
    content <- inComment
    return $ CoupledDoc
               (if start !! 2 == '!'
                  then Inner
                  else Outer) $ init . init $ content

docComment :: Parser Token
docComment = oneLineDocComment <|> multiLineDocComment <?> ""

reservedName :: Parser TokenPos
reservedName = choice . map keywordParser $ sort $ map snd keywordTable
  where
    keywordParser name = withPos $ do
      n <- try $ string name
      notFollowedBy $ satisfy isSymbolLetter
      return $ Keyword $ backwardLookup keywordTable n

attributeContent :: Parser Attribute
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

attribute :: Parser Token
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

charConvertTable :: [(Char, Char)]
charConvertTable = [ ('n', '\n')
                   , ('r', '\r')
                   , ('t', '\t')
                   , ('\\', '\\')
                   , ('0', '\0')
                   , ('\'', '\'')
                   , ('"', '"')
                   ]

stringToInt :: Int -> String -> Int
stringToInt base = foldl1 (\x y -> x * base + y) . map digitToInt

hexStringToInt :: String -> Int
hexStringToInt = stringToInt 16

upTo :: Int -> Parser a -> Parser [a]
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
         <|> try (char '\\') *> fmap (forwardLookup charConvertTable)
                                  (oneOf . map fst $ charConvertTable)
         <|> satisfy isAscii

character :: Parser Char
character = char '\'' `around` inChar

byte :: Parser Char
byte = try (char 'b') *> char '\'' `around` inByte

indentSkipper :: Parser ()
indentSkipper = optional $ try $ string "\\\n" *> simpleSpaces

seqLit :: Parser Char -> Parser String
seqLit c = char '"' *> indentSkipper *> manyTill (c <* indentSkipper) (char '"')

stringLit :: Parser String
stringLit = seqLit inChar

byteStringLit :: Parser String
byteStringLit = try (char 'b') *> seqLit inByte

rawSeqLit :: ParsecT String Int Identity a -> Parser [a]
rawSeqLit c = changeState (const ()) (const 0) $
  try (char 'r') *> (do
                       many (char '#' >> modifyState (1 +))
                       char '"'
                       hashCount <- getState
                       manyTill c (try $ char '"' >> count hashCount (char '#')))

rawStringLit :: Parser String
rawStringLit = rawSeqLit anyChar

rawByteStringLit :: Parser String
rawByteStringLit = try (char 'b') *> rawSeqLit (satisfy isAscii)

stringLiterals :: Parser TokenPos
stringLiterals = choice . map (withPos . fmap Literal) $
  [ fmap ByteChar $ byte
  , fmap ByteString $ byteStringLit
  , fmap ByteString $ rawByteStringLit
  , fmap UnicodeChar $ character
  , fmap UnicodeString $ stringLit
  , fmap UnicodeString $ rawStringLit
  ]

intLit :: Int -> Parser Char -> Parser Literal
intLit base digitLetters =
  do
    digits <- many1 $ (optional (char '_')) `around` digitLetters
    suffix <- (fmap . fmap) (backwardLookup integerSuffixTable) $
                optionMaybe $ choice $ map (try . string . snd) integerSuffixTable
    return $ IntLit suffix $ toInteger $ stringToInt base digits

decLit :: Parser Literal
decLit = intLit 10 digit

hexLit :: Parser Literal
hexLit = intLit 16 $ satisfy isHexDigit

octLit :: Parser Literal
octLit = intLit 8 $ satisfy (`elem` ['0' .. '7'])

binLit :: Parser Literal
binLit = intLit 2 $ satisfy (`elem` ['0', '1'])

intLits :: Parser Literal
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

floatLit :: Parser Literal
floatLit =
  do
    digits <- many1 $ (optional (char '_')) `around` digit
    (do
       try (char '.' <* lookAhead digit)
       digitsAfter <- many1 $ (optional (char '_')) `around` digit
       ex <- optionMaybe expPart
       suffix <- (fmap . fmap) (backwardLookup floatSuffixTable) $
                   optionMaybe $ choice $ map (try . string . snd) floatSuffixTable
       num <- return $ digits ++ ['.'] ++ digitsAfter
       num' <- if ex == Nothing
                 then return num
                 else return (num ++ ['e'] ++ show (fromJust ex))
       return $ FloatLit suffix $ if suffix == Just F32
                                    then Left $ read num'
                                    else Right $ read num'
     <|> do
       try (char '.' <* notFollowedBy (satisfy isSymbolStart))
       return $ FloatLit Nothing (Right $ read digits)
     <|> do
       ex <- expPart
       suffix <- (fmap . fmap) (backwardLookup floatSuffixTable) $
                   optionMaybe $ choice $ map (try . string . snd) floatSuffixTable
       num <- return $ digits ++ ['e'] ++ show ex
       return $ FloatLit suffix $ if suffix == Just F32
                                    then Left $ read num
                                    else Right $ read num)

lifeTime :: Parser LifeTime
lifeTime = do
             try $ char '\'' *> string "static"
             return StaticLife
           <|> do
             symb <- try $ char '\'' *> symbol <* notFollowedBy (char '\'')
             return $ DynamicLife symb

parserize :: (a, String) -> Parser a
parserize (val, str) = try $ string str *> return val

combineTrivial :: (a -> Token) -> [(a, String)] -> Parser TokenPos
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
