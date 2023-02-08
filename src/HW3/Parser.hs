module HW3.Parser
  ( parse
  , findFunName
  ) where

import HW3.Base (HiFun (..), HiValue (..), HiExpr (..), HiAction(..))

import Data.Void (Void)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor (($>))
import Data.Text.Internal.Read (hexDigitToInt)

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Text.Megaparsec ((<|>))
import Text.Megaparsec.Error (ParseErrorBundle)

import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E
import Data.Tuple (swap)

type Parser = MP.Parsec Void String

lexeme :: Parser a -> Parser a
lexeme = L.lexeme C.space

str :: String -> Parser String
str = L.symbol C.space

eps :: Parser ()
eps = C.string "" $> ()

between :: String -> String -> Parser a -> Parser a
between open close = MP.between (str open) (str close)

seqBetween :: String -> String -> Parser a -> Parser [a]
seqBetween open close entry = between open close (entry `MP.sepBy` str ",")

funStrMap :: NonEmpty (String, HiFun)
funStrMap =
  -- This could be auto sorted in reverse lexical order for correct greedy parsing, but I dont have time for that
  -- This also could be auto generated via Show since only `cd` and `mkdir` are odd
  -- Mb this should've been placed to Base 
  ("div", HiFunDiv) :|
  [ ("mul", HiFunMul)
  , ("add", HiFunAdd)
  , ("sub", HiFunSub)
  , ("and", HiFunAnd)
  , ("or", HiFunOr)
  , ("not-less-than", HiFunNotLessThan)
  , ("not-greater-than", HiFunNotGreaterThan)
  , ("not-equals", HiFunNotEquals)
  , ("not", HiFunNot)
  , ("less-than", HiFunLessThan)
  , ("greater-than", HiFunGreaterThan)
  , ("equals", HiFunEquals)
  , ("if", HiFunIf)
  , ("length", HiFunLength)
  , ("to-upper", HiFunToUpper)
  , ("to-lower", HiFunToLower)
  , ("reverse", HiFunReverse)
  , ("trim", HiFunTrim)
  , ("list", HiFunList)
  , ("range", HiFunRange)
  , ("fold", HiFunFold)
  , ("pack-bytes", HiFunPackBytes)
  , ("unpack-bytes", HiFunUnpackBytes)
  , ("zip", HiFunZip)
  , ("unzip", HiFunUnzip)
  , ("encode-utf8", HiFunEncodeUtf8)
  , ("decode-utf8", HiFunDecodeUtf8)
  , ("serialise", HiFunSerialise)
  , ("deserialise", HiFunDeserialise)
  , ("read", HiFunRead)
  , ("write", HiFunWrite)
  , ("mkdir", HiFunMkDir)
  , ("cd", HiFunChDir)
  , ("parse-time", HiFunParseTime)
  , ("rand", HiFunRand)
  , ("echo", HiFunEcho)
  , ("count", HiFunCount)
  , ("keys", HiFunKeys)
  , ("values", HiFunValues)
  , ("invert", HiFunInvert)
  ]
  
findFunName :: HiFun -> Maybe String
findFunName fun = lookup fun $ map swap $ NE.toList funStrMap

hiFun :: Parser HiFun
hiFun = foldl1 (<|>) $ NE.map toParser funStrMap
  where
    toParser (s, f) = str s $> f

hiNumber :: Parser Rational
hiNumber = lexeme $ L.signed eps (toRational <$> L.scientific)

hiBool :: Parser Bool
hiBool = str "true" $> True <|> str "false" $> False

hiNull :: Parser HiValue
hiNull = str "null" $> HiValueNull

hiString :: Parser T.Text
hiString = T.pack <$> lexeme (C.char '"' >> MP.manyTill L.charLiteral (C.char '"'))

hiList :: Parser HiExpr
hiList = HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> seqBetween "[" "]" hiOpExpr

hiBytes :: Parser BS.ByteString
hiBytes = BS.pack <$> between "[#" "#]" (MP.some byte)
  where
    byte = (\a b -> toEnum $ a * 16 + b) <$> digit <*> digit <* C.space -- Mb space1 here?
    digit = hexDigitToInt <$> C.hexDigitChar

hiAction :: Parser HiAction
hiAction = 
  HiActionCwd <$ str "cwd"
  <|> HiActionNow <$ str "now"

hiValue :: Parser HiValue
hiValue =
  HiValueNumber <$> hiNumber
  <|> HiValueFunction <$> hiFun
  <|> HiValueBool <$> hiBool
  <|> hiNull
  <|> HiValueString <$> hiString
  <|> HiValueBytes <$> hiBytes
  <|> HiValueAction <$> hiAction

hiDict :: Parser HiExpr
hiDict = HiExprDict <$> seqBetween "{" "}" dictEntry
  where
    dictEntry = (,) <$> hiOpExpr <*> (str ":" *> hiOpExpr)

hiExpr :: Parser HiExpr
hiExpr = do
  val <- HiExprValue <$> hiValue <|> hiList <|> hiDict
  chainl val 
  where
    chainl val = do
      mb <- MP.optional (withArgs val <|> withDot val <|> withBang val)
      case mb of
        Nothing -> return val
        (Just newVal) -> chainl newVal                        
    withArgs expr = HiExprApply expr <$> seqBetween "(" ")" hiOpExpr
    withDot expr = HiExprApply expr <$> (strToArgList <$> (str "." *> lexeme (MP.some C.alphaNumChar)))
    strToArgList = return . HiExprValue . HiValueString . T.pack
    withBang expr = str "!" $> HiExprRun expr

opTable :: [[E.Operator Parser HiExpr]]
opTable =
  [ [ l (str "*") HiFunMul
    , l (MP.try $ lexeme (C.char '/' <* MP.notFollowedBy (C.char '='))) HiFunDiv
    ]
  , [ l (str "+") HiFunAdd
    , l (str "-") HiFunSub
    ]
  , [ n (str ">=") HiFunNotLessThan
    , n (str "<=") HiFunNotGreaterThan
    , n (str ">") HiFunGreaterThan
    , n (str "<") HiFunLessThan
    , n (str "==") HiFunEquals
    , n (str "/=") HiFunNotEquals
    ]
  , [ r (str "&&") HiFunAnd
    ]
  , [ r (str "||") HiFunOr
    ]
  ]
  where
    binary op p fun = op (p $> (\a b -> HiExprApply (HiExprValue $ HiValueFunction fun) [a, b]))
    l = binary E.InfixL
    r = binary E.InfixR
    n = binary E.InfixN


hiOpExpr :: Parser HiExpr
hiOpExpr = E.makeExprParser hiTerm opTable

hiTerm :: Parser HiExpr
hiTerm = between "(" ")" hiOpExpr <|> hiExpr

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = MP.parse (C.space *> hiOpExpr <* MP.eof) ""
