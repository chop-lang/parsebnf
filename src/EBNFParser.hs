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
    | isSpace x      = parse xs
    | isAlpha x      = TTerminal (TermIdentifier {- ADD -}) : {- ADD -}
    | x `elem` "\"'" = TTerminal (TermString {- ADD -}) : {- ADD -}
    | x `elem` ";."  = TTerminal (TermEnd) : parse xs
    | otherwise = case x of '|' -> TTerminal TermAlt     : parse xs
                            ',' -> TTerminal TermComma   : parse xs
                            '=' -> TTerminal TermEquals  : parse xs
                            '-' -> TTerminal TermExclude : parse xs
                            '?' -> TTerminal (TermSpecial {- ADD -}) : {- ADD -}
                            '(' -> if head xs == '*'
                                        then parse {- ADD -}
                                        else TContainer Group {- ADD -}
                            '[' -> TContainer Option {- ADD -}
                            '{' -> TContainer Repetition {- ADD -}
                            _   -> error $ "Invalid character '" ++ [x] ++ "'."
parse [] = []

