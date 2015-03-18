{-|
Module      : EBNFInterpreter
Description : EBNF-AST-interpreting utilities
Copyright   : (c) chop-lang, 2015
License     : MIT
Maintainer  : carterhinsley@gmail.com
-}
module EBNFInterpreter where

import Data.List.Split (endBy)
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

constructIRForm :: AST -> IRToken

constructIR :: AST -> IR
constructIR = map constructIRForm . separateForms

