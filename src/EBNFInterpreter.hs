{-|
Module      : EBNFInterpreter
Description : EBNF-AST-interpreting utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module EBNFInterpreter where

import Data.Text (Text(..))
import EBNFParser ( ContainerType(..)
                  , Terminal(..)
                  , Token(..)
                  , AST )

data FTerminal = FTermExclude
               | FTermMultiple
               | FTermNumber Text
               | FTermIdentifier Text
               | FTermSpecial Text
               | FTermString Text
               deriving (Show)

data FToken = FTContainer ContainerType FormContent
            | FTTerminal FTerminal
            deriving (Show)

type FAlternation = [FToken]

type FormContent = [FAlternation]

type IR = (Text, FormContent)

