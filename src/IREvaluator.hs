{-|
Module      : IREvaluator
Description : EBNF IR evaluation
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module IREvaluator where

import Data.Text (Text(..))
import IRGenerator ( IRTerminal(..)
                   , IRToken(..)
                   , IRAlternation
                   , IRContent
                   , IRForm
                   , IR )

type PContainerType = Text

type PTerminal = (Text, Text)

data PToken = PTContainer PContainerType PAST
            | PTTerminal PTerminal
            deriving (Eq, Show)

type PAST = [PForm]

