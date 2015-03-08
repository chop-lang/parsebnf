{-|
Module      : CLI
Description : Command-line evocation argument processing
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module CLI where

import System.Environment (getArgs)

-- | Evaluate command-line arguments
evalArgs :: IO [()]
evalArgs = do
    args <- getArgs
    mapM putStrLn args

