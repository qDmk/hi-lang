module Main
  ( main
  ) where

import HW3.Parser (parse)
import HW3.Evaluator (eval)
import HW3.Pretty (prettyValue)
import HW3.Action (runHIO, HiPermission(..))

import Control.Monad.Trans.Class (lift)
import Data.Set (fromList)
import Prettyprinter (annotate, pretty, (<+>), Doc, line)
import Prettyprinter.Render.Terminal (putDoc, color, Color(..))
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Megaparsec.Error (errorBundlePretty) 

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just input -> do
          lift $ case parse input of
            (Left parseError) -> putDoc $ prettyError "Parse error:" <> line <> pretty (errorBundlePretty parseError)
            (Right expr) -> do
              mbVal <- runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              putDoc $ case mbVal of
                (Left evalError) -> prettyError "Eval error:" <+> prettyShow evalError <> line
                (Right val) -> prettyValue val <> line
          loop
    
    prettyError str = annotate (color Red) (pretty str)
    
    prettyShow :: Show a => a -> Doc ann
    prettyShow = pretty . show
    
