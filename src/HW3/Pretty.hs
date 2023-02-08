{-# LANGUAGE TypeApplications #-}

module HW3.Pretty
  ( prettyValue
  ) where

import HW3.Base (HiValue(..), HiAction(..))
import HW3.Parser (findFunName)

import Data.Ratio (numerator, denominator)
import Data.Scientific (fromRationalRepetendUnlimited, floatingOrInteger)
import Prettyprinter (Doc, pretty, annotate, (<+>), encloseSep, viaShow)
import Prettyprinter.Render.Terminal (AnsiStyle, color, Color(..))
import Data.Char (toLower)

import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)

prettySeqBetween :: String -> String -> String -> (a -> Doc ann) -> [a] -> Doc ann
prettySeqBetween open close sep f list = encloseSep (pretty open) (pretty close) (pretty sep) (map f list)

prettyBytes :: BS.ByteString -> Doc AnsiStyle
prettyBytes bytes = annotate (color Cyan) $ prettySeqBetween "[# " " #]" " " prettyByte (BS.unpack bytes)
 where
   prettyByte byte = pretty $ map toHexDigit [fromEnum byte `div` 16, fromEnum byte `mod` 16]
   toHexDigit x = hexDigits !! (x `mod` length hexDigits)
   hexDigits = ['0'..'9'] ++ ['a'..'f']

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueFunction fun) = annotate (color Blue) $ pretty $ fromMaybe (show fun) $ findFunName fun
prettyValue (HiValueBool bool) = annotate (color Cyan) $ pretty $ map toLower $ show bool
prettyValue HiValueNull = annotate (color Red) $ pretty "null"
prettyValue (HiValueString str) = annotate (color Cyan) $ viaShow str
prettyValue (HiValueList list) = annotate (color Cyan) $ prettySeqBetween "[ " " ]" ", " prettyValue (toList list)
prettyValue (HiValueBytes bytes) = prettyBytes bytes
prettyValue (HiValueAction action) = annotate (color Yellow) prettyAction
  where
    prettyAction = case action of
      HiActionCwd -> pretty "cwd"
      HiActionNow -> pretty "now"
      HiActionRead path -> "read" `withStrArg` path
      HiActionWrite path bytes -> "write" `withArgs` [prettyString path, prettyBytes bytes]
      HiActionMkDir path -> "mkdir" `withStrArg` path
      HiActionChDir path -> "cd" `withStrArg` path
      HiActionRand from to -> "rand" `withArgs` map pretty [from, to]
      HiActionEcho text -> "echo" `withStrArg` T.unpack text
    prettyString path = annotate (color Cyan) $ viaShow path
    a `withArgs` args = pretty a <> encloseSep (pretty "(") (pretty ")") (pretty ", ") args
    a `withStrArg` arg = a `withArgs` [prettyString arg]
prettyValue (HiValueTime time) = annotate (color Green) $ viaShow time
prettyValue (HiValueDict dict) = annotate (color Cyan) $ prettySeqBetween "{ " " }" ", " prettyEntry (M.toList dict)
  where
    prettyEntry (k, v) = prettyValue k <> pretty ":" <+> prettyValue v 
prettyValue (HiValueNumber num) = annotate (color Cyan) $ prettyRational num
  where
    prettyRational :: Rational -> Doc AnsiStyle
    prettyRational r =
      case fromRationalRepetendUnlimited r of
        (s, Nothing) -> case floatingOrInteger s of
          (Left double) -> pretty @Double double -- Mb ruins precision here
          (Right int) -> pretty @Integer int
        _ -> asFrac
        where
          n = numerator r
          d = denominator r
          (nQuot, nRem) = quotRem n d
          asFrac 
            | nQuot == 0 = pretty nRem <> pretty "/" <> pretty d
            | otherwise = 
              pretty nQuot 
              <+> pretty (if nQuot >= 0 then "+" else "-") 
              <+> pretty (abs nRem) <> pretty "/" <> pretty d
