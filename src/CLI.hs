{-|
Module      : CLI
Description : Command-line evocation argument processing
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module CLI
( ArgResult(..)
, processArgs
) where

import System.Environment (getArgs)

-- | Contains data gathered from command-line arguments
data ArgResult = ParseData { ebnfFile :: IO String, input :: IO String }

-- | Retrieve command-line arguments, evaluate them, then wrap them back up in
-- the IO monad.
processArgs :: IO [ArgResult]
processArgs = do
    args <- getArgs
    if null args
        then error "You must supply an EBNF file. RTFM for more info."
        else return . evalArgs $ args

-- | Evaluate arguments and return some ArgResults containing information about
-- the arguments.
evalArgs :: [String] -> [ArgResult]
evalArgs [filePath] =
    [ ParseData { ebnfFile = readFile filePath, input = getContents } ]
evalArgs (x:_) = error $ "Argument `" ++ x ++ "' unrecognized. Please RTFM."
evalArgs _ = error "Invalid arguments. Please RTFM."

