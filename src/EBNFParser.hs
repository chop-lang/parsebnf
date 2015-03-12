{-|
Module      : EBNFParser
Description : EBNF-parsing utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
{-# LANGUAGE OverloadedStrings #-}
module EBNFParser
( ContainerType(..)
, Terminal(..)
, Token(..)
, AST
, parse
)where

import Data.Char (isAlpha, isAlphaNum, isSpace)
import qualified Data.List as List (findIndex)
import Data.Maybe (fromMaybe)
import Data.Text hiding (drop, head, length, tail, takeWhile)
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

data Token = TContainer ContainerType AST
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

getTokenContents :: String -> String
getTokenContents haystack@(x:xs) =
    let opening = x
        closing | x == '(' = ')'
                | x == '[' = ']'
                | x == '{' = '}'
    in getContained opening closing xs 1
  where getContained :: Char -> Char -> String -> Int -> String
        getContained opening closing haystackTail@(x:xs) depth
            | x == opening =
                x : (getContained opening closing xs $ succ depth)
            | x == closing =
                if depth == 1
                    then []
                    else x : (getContained opening closing xs $ pred depth)
            | otherwise =
                if x `elem` "\"'"
                    then let (string, rest) = ( splitOnFirst (pack [x])
                                              . pack
                                              $ xs )
                         in [x]
                         ++ unpack string
                         ++ [x]
                         ++ getContained opening closing (unpack rest) depth
                    else x : getContained opening closing xs depth
        getContained opening closing _ _ =
            error $ "Unmatched '"
                 ++ [opening]
                 ++ "', expected '"
                 ++ [closing]
                 ++ "'." 

parseContainer :: String -> (AST, String)
parseContainer haystack =
    let contents = getTokenContents haystack
    in (parse contents, drop ((+2) . length $ contents) haystack)

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
                    '(' -> if head xs == '*'
                                then parse
                                   . unpack
                                   . snd
                                   . splitOnFirst "*)"
                                   . pack
                                   . tail
                                   $ xs
                                else let (content, rest) = parseContainer ebnf
                                     in TContainer Group content : parse rest
                    '[' -> let (content, rest) = parseContainer ebnf
                           in TContainer Option content : parse rest
                    '{' -> let (content, rest) = parseContainer ebnf
                           in TContainer Repetition content : parse rest
                    ')' -> error "Unexpected ')'. Did you leave out a '('?"
                    ']' -> error "Unexpected ']'. Did you leave out a '['?"
                    '}' -> error "Unexpected '}'. Did you leave out a '{'?"
                    _   -> error $ "Invalid character '" ++ [x] ++ "'."
parse [] = []

