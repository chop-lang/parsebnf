{-|
Module      : EBNFParser
Description : EBNF-parsing utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module EBNFParser where

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

