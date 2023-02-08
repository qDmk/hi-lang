{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module HW3.Evaluator
  ( eval
  ) where

import HW3.Base 
  ( HiFun (..)
  , HiValue (..)
  , HiExpr (..)
  , HiError (..)
  , HiAction (..)
  , HiMonad
  , runAction
  )

import Prelude hiding (take, drop, length, reverse)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Control.Monad ((<=<), (>=>), forM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, catchE)
import Data.Function ((&))
import Data.Sequence ((<|))
import Data.Word (Word8)
import Data.Foldable (toList)
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Codec.Compression.Zlib (compressWith, compressLevel, defaultCompressParams, decompress, bestCompression)
import Codec.Serialise (serialise, deserialiseOrFail)

import qualified Data.Text as T
import qualified Data.Sequence as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Time.Clock as CT
import qualified Data.Map.Lazy as M
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

type Calc m = HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
type Modifier m a b = HiMonad m => a -> ExceptT HiError m b -- I called them after Solidity's modifiers
type ValueModifier m a = Modifier m HiValue a

infixl 3 <|>
(<|>) :: (Monad m) => (a -> ExceptT HiError m b) -> (a -> ExceptT HiError m b) -> (a -> ExceptT HiError m b)
(<|>) left right args = do
  leftCalc <- try left 
  case leftCalc of 
    (Left leftErr) -> do 
      calcRight <- try right
      case calcRight of 
        (Left _) -> throwE leftErr
        (Right val) -> return val
    (Right val) -> return val
  where
    try m = catchE (Right <$> m args) (return . Left) -- modified version of tryE from transformers-0.6.0.6

class Monoid l => ListLike l where
  type Elem l
  toValue :: l -> HiValue
--  elemToValue :: Elem l -> HiValue -- redundant due to problem described right below
  length :: l -> Int
  at :: l -> Int -> Maybe HiValue  {- If I change this HiValue to (Elem l) everything breaks in `listAt` below.
                                      GHC says something about "non-injective type families" and cannot deduce types
                                      I had not time to deal with it, but I'd like to know the solution for this -}
  take :: Int -> l -> l
  drop :: Int -> l -> l
  reverse :: l -> l
  fold :: (a -> HiValue -> a) -> a -> l -> a -- same reason

  calcConcat :: ValueModifier m l -> Calc m
  calcConcat modifier = binary $ \a b -> do
    (left :: l) <- modifier a
    right <- modifier b
    return $ toValue $ left <> right

  calcReplicate :: ValueModifier m l -> Calc m
  calcReplicate modifier = binary $ \a b -> do
    (list :: l) <- modifier a
    number <- onlyNonNegative b
    return $ toValue $ stimes (floor number :: Integer) list

  calcLength :: ValueModifier m l -> Calc m
  calcLength modifier = unary $ modifier >=> (return . HiValueNumber . toRational . length)

  calcReverse :: ValueModifier m l -> Calc m
  calcReverse modifier = unary $ modifier >=> (return . toValue . reverse)

  calcCount :: ValueModifier m l -> Calc m
  calcCount modifier = unary $ modifier >=> (return . HiValueDict . mapValues . fold update M.empty)
    where
      update :: M.Map HiValue Int -> HiValue -> M.Map HiValue Int
      update m e = M.alter f e m
      f = \case
        Nothing -> Just 1
        (Just num) -> Just $ num + 1
      mapValues = M.map (HiValueNumber . toRational)

  calcCall :: l -> Calc m
  calcCall list = listAt <|> listSlice
    where
      listAt = unary $ onlyIntArg >=> (return . fromMaybe HiValueNull . at list)
      --listAt = unary $ onlyIntArg >=> (return . maybe HiValueNull elemToValue . (at list)) -- see above
      
      listSlice = binary $ \a b -> do
        from <- (onlyIntArg <|> onlyNullArg 0) a
        to <- (onlyIntArg <|> onlyNullArg len) b
        return $ toValue $ slice (fixNegative from) (fixNegative to)

      slice from to
        | from > to = mempty
        | otherwise = subList (from `max` 0) (to `min` len)

      subList from to = list & drop from & take (to - from)

      fixNegative x
        | x < 0 = len + x
        | otherwise = x

      len = length list

instance ListLike T.Text where
  type Elem T.Text = Char
  toValue = HiValueString
--  elemToValue = HiValueString . T.singleton
  length = T.length
  text `at` pos
    | 0 <= pos && pos < T.length text = Just $ HiValueString $ T.singleton $ T.head $ T.drop pos text
    | otherwise = Nothing
  take = T.take
  drop = T.drop
  reverse = T.reverse
  fold f = T.foldl (\a b -> f a (HiValueString $ T.singleton b))

instance ListLike (S.Seq HiValue) where
  type Elem (S.Seq HiValue) = HiValue
  toValue = HiValueList
--  elemToValue = id
  length = S.length
  at = flip S.lookup
  take = S.take
  drop = S.drop
  reverse = S.reverse
  fold = foldl

instance ListLike BS.ByteString where
  type Elem BS.ByteString = Word8
  toValue = HiValueBytes
--  elemToValue = HiValueNumber . toRational
  length = BS.length
  bytes `at` pos
    | 0 <= pos && pos < BS.length bytes = Just $ HiValueNumber $ toRational $ BS.index bytes pos
    | otherwise = Nothing
  take = BS.take
  drop = BS.drop
  reverse = BS.reverse
  fold f = BS.foldl (\a b -> f a (HiValueNumber $ toRational b))


eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval e = runExceptT (eval' e)

eval' :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
eval' (HiExprValue val) = return val
eval' (HiExprRun actionExpr) = eval' actionExpr >>= onlyActionArg >>= lift . runAction
eval' (HiExprDict list) = HiValueDict . M.fromList <$> mapM evalPair list
  where
    evalPair (a, b) = (,) <$> eval' a <*> eval' b
eval' (HiExprApply funExpr argsExpr) = eval' funExpr >>= (`calc` argsExpr)
  where
    calc = \case
      (HiValueFunction fun) -> calcFun fun
      (HiValueString str) -> calcCall str
      (HiValueList list) -> calcCall list
      (HiValueBytes bytes) -> calcCall bytes
      (HiValueDict dict) -> unary $ return . fromMaybe HiValueNull . (dict M.!?)
      _ -> const $ throwE HiErrorInvalidFunction


calcFun :: HiFun -> Calc m
calcFun = \case
  HiFunAdd -> simpleNumericBinary (+)
    <|> calcConcat onlyStringArg
    <|> calcConcat onlyListArg
    <|> calcConcat onlyBytesArg
    <|> addTime
  HiFunSub -> simpleNumericBinary (-) <|> diffTime
  HiFunMul -> simpleNumericBinary (*)
    <|> calcReplicate onlyStringArg
    <|> calcReplicate onlyListArg
    <|> calcReplicate onlyBytesArg
  HiFunDiv -> numericBinary safeDiv <|> stringPathConcat
    where
      safeDiv _ 0 = throwE HiErrorDivideByZero
      safeDiv a b = return $ a / b

  HiFunNot -> unary (return . HiValueBool . not <=< onlyBoolArg)
  HiFunAnd -> lazyBinary not
  HiFunOr -> lazyBinary id

  HiFunLessThan -> cmpBinary (<)
  HiFunGreaterThan -> cmpBinary (>)
  HiFunEquals -> cmpBinary (==)
  HiFunNotLessThan -> cmpBinary (>=)
  HiFunNotGreaterThan -> cmpBinary (<=)
  HiFunNotEquals -> cmpBinary (/=)

  HiFunIf -> calcIf

  HiFunLength ->
    calcLength onlyStringArg
    <|> calcLength onlyListArg
    <|> calcLength onlyBytesArg
  HiFunToUpper -> stringUnary (HiValueString . T.toUpper)
  HiFunToLower -> stringUnary (HiValueString . T.toLower)
  HiFunReverse ->
    calcReverse onlyStringArg
    <|> calcReverse onlyListArg
    <|> calcReverse onlyBytesArg
  HiFunTrim -> stringUnary (HiValueString . T.strip)

  HiFunList -> fmap (HiValueList . S.fromList) . mapM eval'
  HiFunRange -> calcRange
  HiFunFold -> calcFold

  HiFunPackBytes -> calcPack
  HiFunUnpackBytes -> calcUnpack
  HiFunEncodeUtf8 -> unary $ onlyStringArg >=> return . HiValueBytes . encodeUtf8
  HiFunDecodeUtf8 -> unary $ onlyBytesArg >=> return . either (const HiValueNull) HiValueString . decodeUtf8'
  HiFunZip -> calcZip
  HiFunUnzip -> calcUnzip
  HiFunSerialise -> unary $ return . HiValueBytes . LBS.toStrict . serialise
  HiFunDeserialise -> unary $ onlyBytesArg >=>
    either
      (const $ throwE HiErrorInvalidArgument)
      return
    . (deserialiseOrFail . LBS.fromStrict)

  HiFunRead -> pathActionUnary HiActionRead
  HiFunWrite -> actionBinary $ \a b -> do
    path <- onlyStringArg a
    val <- onlyStringArg b
    return $ HiActionWrite (T.unpack path) (encodeUtf8 val)
  HiFunMkDir -> pathActionUnary HiActionMkDir
  HiFunChDir -> pathActionUnary HiActionChDir
  HiFunParseTime -> stringUnary (maybe HiValueNull HiValueTime . readMaybe . T.unpack)
  HiFunRand -> actionBinary $ \a b -> do
    from <- onlyIntArg a
    to <- onlyIntArg b
    return $ HiActionRand from to
  HiFunEcho -> actionUnary $ onlyStringArg >=> return . HiActionEcho

  HiFunKeys -> dictUnaryToList M.keys
  HiFunValues -> dictUnaryToList M.elems
  HiFunCount ->
    calcCount onlyStringArg
    <|> calcCount onlyListArg
    <|> calcCount onlyBytesArg
  HiFunInvert -> dictUnary $ HiValueDict . M.map (HiValueList . S.fromList) . invertMap

onlyFunctionArg :: ValueModifier m HiFun
onlyFunctionArg (HiValueFunction fun) = return fun
onlyFunctionArg _ = throwE HiErrorInvalidArgument

onlyNumberArg :: ValueModifier m Rational
onlyNumberArg (HiValueNumber num) = return num
onlyNumberArg _ = throwE HiErrorInvalidArgument

onlyIntArg :: ValueModifier m Int
onlyIntArg = onlyNumberArg >=> onlyInteger >=> onlyInt
  where
    onlyInteger rational
      | denominator rational == 1 = return $ numerator rational
      | otherwise = throwE HiErrorInvalidArgument
         
    onlyInt integer
      | toEnum (minBound :: Int) <= integer && integer <= toEnum (maxBound :: Int) = return $ fromEnum integer
      | otherwise = throwE HiErrorInvalidArgument
      

onlyNonNegative :: ValueModifier m Rational
onlyNonNegative = onlyNumberArg >=> nonNegative
  where
    nonNegative x 
      | x >= 0 = return x
      | otherwise = throwE HiErrorInvalidArgument

onlyWord8Arg :: ValueModifier m Word8
onlyWord8Arg = onlyIntArg >=> onlyWord
  where
    onlyWord int
      | 0 <= int && int < 256 = return $ toEnum int
      | otherwise = throwE HiErrorInvalidArgument

onlyBoolArg :: ValueModifier m Bool
onlyBoolArg (HiValueBool bool) = return bool
onlyBoolArg _ = throwE HiErrorInvalidArgument

onlyStringArg :: ValueModifier m T.Text
onlyStringArg (HiValueString str) = return str
onlyStringArg _ = throwE HiErrorInvalidArgument

onlyListArg :: ValueModifier m (S.Seq HiValue)
onlyListArg (HiValueList list) = return list
onlyListArg _ = throwE HiErrorInvalidArgument

onlyBytesArg :: ValueModifier m BS.ByteString
onlyBytesArg (HiValueBytes bytes) = return bytes
onlyBytesArg _ = throwE HiErrorInvalidArgument

onlyActionArg :: ValueModifier m HiAction
onlyActionArg (HiValueAction action) = return action
onlyActionArg _ = throwE HiErrorInvalidArgument

onlyTimeArg :: ValueModifier m CT.UTCTime
onlyTimeArg (HiValueTime time) = return time
onlyTimeArg _ = throwE HiErrorInvalidArgument

onlyDictArg :: ValueModifier m (M.Map HiValue HiValue)
onlyDictArg (HiValueDict dict) = return dict
onlyDictArg _ = throwE HiErrorInvalidArgument

onlyNullArg :: a -> ValueModifier m a
onlyNullArg a HiValueNull = return a
onlyNullArg _ _ = throwE HiErrorInvalidArgument

unary :: (HiValue -> ExceptT HiError m HiValue) -> Calc m
unary f [a] = (f <=< eval') a
unary _ _ = throwE HiErrorArityMismatch

stringUnary :: (T.Text -> HiValue) -> Calc m
stringUnary f = unary (return . f <=< onlyStringArg)

actionUnary :: (HiValue -> ExceptT HiError m HiAction) -> Calc m
actionUnary action = unary $ action >=> return . HiValueAction

pathActionUnary :: (FilePath -> HiAction) -> Calc m
pathActionUnary action = actionUnary $ onlyStringArg >=> return . action . T.unpack

dictUnary :: (M.Map HiValue HiValue -> HiValue) -> Calc m
dictUnary f = unary $ onlyDictArg >=> return . f

dictUnaryToList :: (M.Map HiValue HiValue -> [HiValue]) -> Calc m
dictUnaryToList f = dictUnary $ HiValueList . S.fromList . f

binary :: (HiValue -> HiValue -> ExceptT HiError m HiValue) -> Calc m
binary f [a, b] = do
  aVal <- eval' a
  bVal <- eval' b
  f aVal bVal
binary _ _ = throwE HiErrorArityMismatch

numericBinary :: (Rational -> Rational -> ExceptT HiError m Rational) -> Calc m
numericBinary f = binary $ \a b -> do
  aNum <- onlyNumberArg a
  bNum <- onlyNumberArg b
  HiValueNumber <$> f aNum bNum

simpleNumericBinary :: (Rational -> Rational -> Rational) -> Calc m
simpleNumericBinary f = numericBinary (\a b -> return $ f a b)

lazyBinary :: (Bool -> Bool) -> Calc m
lazyBinary logic [a, b] = do
  left <- eval' a
  if logic $ toBool left then do
    return left
  else do
    eval' b
  where
    toBool = \case
      HiValueNull -> False
      (HiValueBool False) -> False
      _ -> True
lazyBinary _ _ = throwE HiErrorArityMismatch


cmpBinary :: (HiValue -> HiValue -> Bool) -> Calc m
cmpBinary f = binary (\a b -> return $ HiValueBool $ f a b)

actionBinary :: (HiValue -> HiValue -> ExceptT HiError m HiAction) -> Calc m
actionBinary f = binary $ \a b -> HiValueAction <$> f a b

calcIf :: Calc m
calcIf [cond, ifTrue, ifFalse] = do
  boolCond <- (onlyBoolArg <=< eval') cond
  eval' (if boolCond then ifTrue else ifFalse)
calcIf _ = throwE HiErrorArityMismatch


calcRange :: Calc m
calcRange = binary $ \a b -> do
  from <- onlyNumberArg a
  to <- onlyNumberArg b
  return $ HiValueList $ S.fromList $ map HiValueNumber [from..to]


calcFold :: Calc m
calcFold = binary $ \a b -> do
  f <- onlyFunctionArg a
  list <- nonEmpty <=< onlyListArg $ b
  eval' $ foldl1 (foldFun f) $ fmap HiExprValue list
    where
      nonEmpty s = if S.null s then throwE HiErrorArityMismatch else return s
      foldFun f l r = HiExprApply (HiExprValue $ HiValueFunction f) [l, r]

calcPack :: Calc m
calcPack = unary $ \a -> do
  list <- onlyListArg a
  byteSeq <- forM list onlyWord8Arg
  return $ HiValueBytes $ BS.pack $ toList byteSeq
  
calcUnpack :: Calc m
calcUnpack = unary $ \a -> do
  bytes <- onlyBytesArg a
  return $ HiValueList $ BS.foldr (\word list -> toNumber word <| list) S.empty bytes 
  where
    toNumber word = HiValueNumber $ toEnum $ fromEnum word

bytesUnary :: (LBS.ByteString -> LBS.ByteString) -> Calc m
bytesUnary f = unary $ onlyBytesArg >=> return . HiValueBytes . LBS.toStrict . f . LBS.fromStrict
    
calcZip :: Calc m
calcZip = bytesUnary $ compressWith defaultCompressParams { compressLevel = bestCompression }    

calcUnzip :: Calc m
calcUnzip = bytesUnary decompress

stringPathConcat :: Calc m
stringPathConcat = binary $ \a b -> do
  left <- onlyStringArg a
  right <- onlyStringArg b
  return $ HiValueString $ T.concat [left, T.singleton '/', right]

addTime :: Calc m
addTime = binary $ \a b -> do
  time <- onlyTimeArg a
  toAdd <- onlyNumberArg b
  return $ HiValueTime $ CT.addUTCTime (fromRational toAdd) time

diffTime :: Calc m
diffTime = binary $ \a b -> do
  leftTime <- onlyTimeArg a
  rightTime <- onlyTimeArg b
  return $ HiValueNumber $ toRational $ CT.diffUTCTime leftTime rightTime

invertMap :: (Ord v) => M.Map k v -> M.Map v [k]
invertMap = M.fromListWith (++) . map swapWrap . M.toList
  where
    swapWrap (a, b) = (b, [a])
