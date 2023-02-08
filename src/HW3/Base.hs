{-# LANGUAGE DeriveGeneric #-}

module HW3.Base
  ( HiFun(..)
  , HiValue(..)
  , HiExpr(..)
  , HiError(..)
  , HiAction(..)
  , HiMonad
  , runAction
  ) where

import GHC.Generics (Generic)
    
import Data.Text (Text)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Map.Lazy (Map)

import Codec.Serialise.Class (Serialise)

--TODO: remove derivings

data HiFun =
  HiFunAdd
  | HiFunSub
  | HiFunMul
  | HiFunDiv
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiFun


data HiValue =
  HiValueFunction HiFun
  | HiValueNull
  | HiValueBool Bool
  | HiValueNumber Rational
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic)

instance Serialise HiValue

data HiExpr =
  HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show)

data HiError =
  HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show)

data HiAction =
  HiActionRead FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic)
  
instance Serialise HiAction  

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
