{-|
Module      : Main
Description : Entry point for parsebnf
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module Main where

import qualified CLI
import qualified EBNFParser as EBNF

main = do
    argResults <- CLI.processArgs
    ebnfFile <- CLI.ebnfFile . head $ argResults
    print . EBNF.parse $ ebnfFile

