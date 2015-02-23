module CLI where

import Data.List
import System.Environment

myGetArgs :: IO String
myGetArgs = fmap (intercalate " ") getArgs

