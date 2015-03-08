{-|
Module      : Main
Description : Entry point for parsebnf
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module Main where

import qualified CLI

main = do
    CLI.evalArgs

