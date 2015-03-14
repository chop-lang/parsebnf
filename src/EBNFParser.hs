{-|
Module      : EBNFParser
Description : EBNF-parsing utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
{-# LANGUAGE OverloadedStrings #-}
module EBNFParser
( parse
) where

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
              | TermMultiple
              | TermIdentifier Text
              | TermSpecial Text
              | TermString Text
              deriving (Show)

data Token = TContainer ContainerType AST
           | TTerminal Terminal
           deriving (Show)

type AST = [Token]

-- | Returns a tuple containing 2 text values: one containing all the text
-- occurring before the first occurence of the needle value in the haystack
-- value, and another containing all the text occuring after that first needle
-- occurence.
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

-- | Retrieve the contents of the container that the supplied EBNF string
-- begins with. Obviously, the supplied EBNF string must begin with a container
-- opening character ('(', '[', '{', etc.).
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

-- | Generate a tuple containing an AST generated from the string contained
-- within a container item (group, option, repetition, etc.) and the rest of
-- the string containing the EBNF. Argument must begin with a container-opening
-- character ('(', '[', '{', etc.).
parseContainer :: String -> (AST, String)
parseContainer haystack =
    let contents = getTokenContents haystack
    in (parse contents, drop ((+2) . length $ contents) haystack)

-- | Generate an AST of the supplied string containing an EBNF.
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
          case x of '|' -> TTerminal TermAlt      : parse xs
                    ',' -> TTerminal TermComma    : parse xs
                    '=' -> TTerminal TermEquals   : parse xs
                    '-' -> TTerminal TermExclude  : parse xs
                    '+' -> TTerminal TermMultiple : parse xs
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

