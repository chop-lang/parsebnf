{-|
Module      : EBNFParser
Description : EBNF-parsing utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module EBNFParser where

import Data.Char (isAlpha, isSpace)
import Data.Text (Text(..))
import qualified Data.Text as Text

data ContainerType = Group
                   | Option
                   | Repetition
                   deriving (Show)

data Terminal = TermAlt
              | TermComma
              | TermEnd
              | TermEquals
              | TermExclude
              | TermIdentifier Text
              | TermSpecial Text
              | TermString Text
              deriving (Show)

data Token = TContainer ContainerType Token
           | TTerminal Terminal
           deriving (Show)

type AST = [Token]

parse :: String -> AST
parse ebnf@(x:xs)
    | x == '|'              = TTerminal (TermAlt)
                            : parse xs
    | x == ','              = TTerminal (TermComma)
                            : parse xs
    | x == ';' || x == '.'  = TTerminal (TermEnd)
                            : parse xs
    | x == '='              = TTerminal (TermEquals)
                            : parse xs
    | x == '-'              = TTerminal (TermExclude)
                            : parse xs
    | isAlpha x             = TTerminal (TermIdentifier {- ADD -})
                            : {- ADD -}
    | x == '?'              = TTerminal (TermSpecial {- ADD -})
                            : {- ADD -}
    | x == '"' || x == '\'' = TTerminal (TermString {- ADD -})
                            : {- ADD -}
    | x == '('              = if head xs == '*'
                                  then parse {- ADD -}
                                  else TContainer Group {- ADD -}
    | x == '['              = TContainer Option {- ADD -}
    | x == '{'              = TContainer Repetition {- ADD -}
    | isSpace x             = parse xs
    | otherwise             = error $ "Invalid character '" ++ [x] ++ "'."
parse [] = []

