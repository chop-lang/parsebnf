{-|
Module      : IRGenerator
Description : EBNF-AST-to-IR conversion
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module IRGenerator where

import Data.List.Split (endBy, splitOn)
import Data.Text (Text(..))
import EBNFParser ( ContainerType(..)
                  , Terminal(..)
                  , Token(..)
                  , AST )

data IRTerminal = IRTermExclude
                | IRTermMultiple
                | IRTermNumber Text
                | IRTermIdentifier Text
                | IRTermSpecial Text
                | IRTermString Text
                deriving (Eq, Show)

data IRToken = IRTContainer ContainerType IRContent
             | IRTTerminal IRTerminal
             deriving (Eq, Show)

type IRAlternation = [IRToken]

type IRContent = [IRAlternation]

type IRForm = (Text, IRContent)

type IR = [IRForm]

separateForms :: AST -> [AST]
separateForms ast@((TTerminal (TermIdentifier _)) : _) =
    endBy [TTerminal TermEnd] ast
separateForms (f:_) = error $ "Form " ++ show f ++ " is not an identifier."
separateForms [] = error $ "AST must end with a TermEnd (end terminal) token."

constructIRForm :: AST -> IRForm
constructIRForm ( TTerminal (TermIdentifier termName)
                : TTerminal TermEquals
                : xs ) =
    (termName, constructIRContent xs)
  where constructIRContent :: AST -> IRContent
        constructIRContent = map constructIRAlt . splitOn [TTerminal TermAlt]
          where constructIRAlt :: AST -> IRAlternation
                constructIRAlt [] = []
                constructIRAlt (TContainer containerType ast : xs) =
                    ( IRTContainer containerType . constructIRContent $ ast )
                    : constructIRAlt xs
                constructIRAlt (TTerminal terminal : xs) = 
                    if terminal == TermComma
                        then constructIRAlt xs
                        else (case terminal of
                                  TermExclude      ->
                                      IRTTerminal   IRTermExclude
                                  TermMultiple     ->
                                      IRTTerminal   IRTermMultiple
                                  TermNumber     x ->
                                      IRTTerminal . IRTermNumber
                                                  $ x
                                  TermIdentifier x ->
                                      IRTTerminal . IRTermIdentifier
                                                  $ x
                                  TermSpecial    x ->
                                      IRTTerminal . IRTermSpecial
                                                  $ x
                                  TermString     x ->
                                      IRTTerminal . IRTermString
                                                  $ x
                        ) : constructIRAlt xs

constructIR :: AST -> IR
constructIR = map constructIRForm . separateForms

