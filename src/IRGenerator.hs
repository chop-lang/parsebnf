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

-- | An EBNF form IR terminal.
data IRTerminal = IRTermExclude
                | IRTermMultiple
                | IRTermNumber Text
                | IRTermIdentifier Text
                | IRTermSpecial Text
                | IRTermString Text
                deriving (Eq, Show)

-- | An EBNF form IR token, taking its container type from the EBNF form AST
-- token container types. Applicable terminals should mirror those of the AST
-- in most, if not all, cases.
data IRToken = IRTContainer ContainerType IRContent
             | IRTTerminal IRTerminal
             deriving (Eq, Show)

-- | An EBNF form IR alternation, represented as a series of tokens.
type IRAlternation = [IRToken]

-- | The content of an EBNF form IR, represented as a series of alternations.
type IRContent = [IRAlternation]

-- | Tuple containing a Text object which serves as the identifier that the
-- following IRContent is 'bound' to.
type IRForm = (Text, IRContent)

-- | Internal representation of an EBNF.
type IR = [IRForm]

-- | Separate the forms in an EBNF AST, returning a list of ASTs.
separateForms :: AST -> [AST]
separateForms ast@((TTerminal (TermIdentifier _)) : _) =
    endBy [TTerminal TermEnd] ast
separateForms (f:_) = error $ "Form " ++ show f ++ " is not an identifier."
separateForms [] = error $ "AST must end with a TermEnd (end terminal) token."

-- | Convert a single EBNF AST form (one of the ASTs returned by separateForms)
-- into an IRForm.
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

-- | Convert any valid EBNF AST into an IR.
constructIR :: AST -> IR
constructIR = map constructIRForm . separateForms

