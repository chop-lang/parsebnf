{-|
Module      : IREvaluator
Description : EBNF IR evaluation
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module IREvaluator 
( PTerminal
, PToken(..)
, PAST
, evaluate
, createEvaluator
) where

import Data.Text (Text(..))
import IRGenerator ( IRTerminal(..)
                   , IRToken(..)
                   , IRAlternation
                   , IRContent
                   , IRForm
                   , IR )

data EToken = ETContainer EContent
            | ETTerminal ETerminal
            deriving (Eq, Show)

type EAlternation = [EToken]

type EContent = [EAlternation]
 
type EForm = (Text, EContent)

type ER = [EForm]

-- | A product AST terminal, its first element being a descriptor of the
-- terminal and its second element being the terminal content.
type PTerminal = (Text, Text)

-- | A product AST token, either containing another product AST or comprising
-- a PTerminal.
data PToken = PTContainer PAST
            | PTTerminal PTerminal
            deriving (Eq, Show)

-- | The final product AST of the parsing process.
type PAST = [PToken]

-- | Accept an EBNF IR and some source text and product a product AST.
evaluate :: IR -> Text -> PAST

-- | Accept an EBNF IR and produce an evaluator representation.
createEvaluator :: IR -> ER

