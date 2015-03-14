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

data ArgResult = ParseData { ebnfFile :: IO String
                           , input :: IO String
                           }

-- | Process command-line arguments
processArgs :: IO [ArgResult]
processArgs = do
    args <- getArgs
    if null args
        then error "You must supply an EBNF file. RTFM for more info."
        else return . evalArgs $ args

evalArgs :: [String] -> [ArgResult]
evalArgs [filePath] =
    [ ParseData { ebnfFile = readFile filePath
                , input = getContents
                }
    ]
evalArgs (x:_) = error $ "Argument `" ++ x ++ "' unrecognized. Please RTFM."
evalArgs _ = error "Invalid arguments. Please RTFM."

