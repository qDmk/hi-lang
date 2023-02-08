{-# LANGUAGE LambdaCase #-}

module HW3.Action
  ( HiPermission(..)
  , PermissionException
  , HiMonad
  , HIO
  , runHIO
  ) where

import HW3.Base (HiValue(..), HiAction(..), HiMonad, runAction)
  
import Control.Exception.Base (Exception, throwIO) 
import Data.Text (pack)
import Data.Sequence (fromList)
import Data.Time.Clock (getCurrentTime)
import Data.Text.Encoding (decodeUtf8')
import qualified Data.Text.IO as TIO
import Data.Set (Set, member)
import qualified Data.ByteString as BS
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, listDirectory, doesFileExist, setCurrentDirectory, doesDirectoryExist)
import System.Random

data HiPermission =
  AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Eq, Ord)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a }

instance Functor HIO where
  fmap f (HIO a) = HIO $ \s -> fmap f (a s)

instance Applicative HIO where
  pure a = HIO $ \_ -> pure a
  (HIO f) <*> (HIO a) = HIO $ \s -> f s <*> a s

instance Monad HIO where
  (HIO hio) >>= f = HIO $ \s -> hio s >>= (\a -> runHIO (f a) s)

instance HiMonad HIO where
  runAction = \case
    HiActionCwd -> withRead $ pathToHiValueString <$> getCurrentDirectory
    (HiActionRead path) -> withRead $ do
      isFile <- doesFileExist path
      isDir <- doesDirectoryExist path
      if isFile then do
        fileBytes <- BS.readFile path
        return $ either (const $ HiValueBytes fileBytes) HiValueString $ decodeUtf8' fileBytes
      else if isDir then do
        HiValueList . fromList . map pathToHiValueString <$> listDirectory path
      else
        return HiValueNull
    (HiActionWrite path bytes) -> withWrite $ HiValueNull <$ BS.writeFile path bytes
    (HiActionMkDir path) -> withWrite $ HiValueNull <$ createDirectoryIfMissing True path
    (HiActionChDir path) -> withRead $ HiValueNull <$ setCurrentDirectory path
    HiActionNow -> withTime $ HiValueTime <$> getCurrentTime
    (HiActionRand from to) -> anyPermissions $ HiValueNumber . toRational <$> getStdRandom (randomR (from, to))
    (HiActionEcho text) -> withWrite $ HiValueNull <$ TIO.putStrLn text
    where
      pathToHiValueString = HiValueString . pack
      anyPermissions = HIO . const
      withRead = withPermission AllowRead
      withWrite = withPermission AllowWrite
      withTime = withPermission AllowTime
      withPermission permission f = HIO impl
        where
          impl permissions
            | permission `member` permissions = f
            | otherwise = throwIO $ PermissionRequired permission

