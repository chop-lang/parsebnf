{-|
Module      : EBNFParser
Description : EBNF-parsing utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module EBNFParser where

{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isAlpha, isSpace)
import qualified Data.List as List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text as Text

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

splitOnFirst :: Text -> Text -> (Text, Text)
splitOnFirst needle haystack =
    case Text.splitAt location haystack of
        (xs, ys) -> (xs, Text.drop (Text.length needle) ys)
  where location :: Int
        location =
            fromMaybe ( error $ "No occurence of '"
                              ++ unpack needle
                              ++ "' found." )
                      . List.findIndex (needle `Text.isPrefixOf`)
                      . tails
                      $ haystack

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
                            '?' -> let (content, rest) = splitOnFirst "?"
                                                       . pack
                                                       $ xs
                                   in TTerminal (TermSpecial content)
                                    : parse rest
                            '(' -> if head xs == '*'
                                        then parse
                                           . snd
                                           . splitOnFirst "*)"
                                           $ xs
                                        else TContainer Group {- ADD -} : {- ADD -}
                            '[' -> TContainer Option {- ADD -} : {- ADD -}
                            '{' -> TContainer Repetition {- ADD -} : {- ADD -}
                            _   -> error $ "Invalid character '" ++ [x] ++ "'."
parse [] = []

