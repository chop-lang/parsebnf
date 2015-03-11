{-|
Module      : EBNFParser
Description : EBNF-parsing utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
{-# LANGUAGE OverloadedStrings #-}
module EBNFParser where


import Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.List as List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text hiding (drop, length, takeWhile)
import qualified Data.Text

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
    case Data.Text.splitAt location haystack of
        (xs, ys) -> (xs, Data.Text.drop (Data.Text.length needle) ys)
  where location :: Int
        location =
            fromMaybe ( error $ "No occurence of '"
                              ++ unpack needle
                              ++ "' found." )
                      . List.findIndex (needle `Data.Text.isPrefixOf`)
                      . tails
                      $ haystack

parse :: String -> AST
parse ebnf@(x:xs)
    | isSpace x      = parse xs
    | isAlpha x      = let identifier = pack . takeWhile isAlphaNum $ ebnf
                       in (TTerminal $ TermIdentifier identifier)
                        : (parse . drop (Data.Text.length identifier) $ ebnf)
    | x `elem` "\"'" = let (content, rest) = splitOnFirst (pack [x])
                                           . pack
                                           $ xs
                       in (TTerminal $ TermString content)
                        : (parse . unpack $ rest)
    | x `elem` ";."  = TTerminal (TermEnd) : parse xs
    | otherwise =
          case x of '|' -> TTerminal TermAlt     : parse xs
                    ',' -> TTerminal TermComma   : parse xs
                    '=' -> TTerminal TermEquals  : parse xs
                    '-' -> TTerminal TermExclude : parse xs
                    '?' -> let (content, rest) = splitOnFirst "?"
                                               . pack
                                               $ xs
                           in (TTerminal $ TermSpecial content)
                            : (parse . unpack $ rest)
--                    '(' -> if head xs == '*'
--                                then parse
--                                   . snd
--                                   . splitOnFirst "*)"
--                                   . tail
--                                   $ xs
--                                else TContainer Group {- ADD -} : {- ADD -}
--                    '[' -> TContainer Option {- ADD -} : {- ADD -}
--                    '{' -> TContainer Repetition {- ADD -} : {- ADD -}
                    _   -> error $ "Invalid character '" ++ [x] ++ "'."
parse [] = []

