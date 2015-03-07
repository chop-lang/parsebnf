{-|
Module      : CLI
Description : Command line evocation argument processing
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module CLI where

import Data.List
import System.Environment

myGetArgs :: IO String
myGetArgs = fmap unwords getArgs

