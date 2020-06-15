{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- - This file defines the main ADT for the Juvix front end language.
-- - This ADT corresponds to the bnf laid out [[https://github.com/cryptiumlabs/juvix/blob/develop/doc/Frontend/syntax.org][here]].
-- - Later a trees that grow version of this will be implemented, so
--   infix functions can better transition across syntax
-- - Note :: The names for the types in =ArrowData= are stored in the
--           =ArrowGen= and not in =NamedType=
module Juvix.Frontend.Types where

import Juvix.Frontend.Types.Base
import Juvix.Library hiding (Product, Sum)

data T

extendType "Type" [] [t|T|] defaultExtType

extendTopLevel "TopLevel" [] [t|T|] defaultExtTopLevel

extendTypeSum "TypeSum" [] [t|T|] defaultExtTypeSum

extendData "Data" [] [t|T|] defaultExtData

extendAlias "Alias" [] [t|T|] defaultExtAlias

extendNamedType "NamedType" [] [t|T|] defaultExtNamedType

extendTypeRefine "TypeRefine" [] [t|T|] defaultExtTypeRefine

extendName "Name" [] [t|T|] defaultExtName

extendArrowSymbol "ArrowSymbol" [] [t|T|] defaultExtArrowSymbol

extendUniverseExpression "UniverseExpression" [] [t|T|] defaultExtUniverseExpression

extendAdt "Adt" [] [t|T|] defaultExtAdt

extendSum "Sum" [] [t|T|] defaultExtSum

extendProduct "Product" [] [t|T|] defaultExtProduct

extendRecord "Record" [] [t|T|] defaultExtRecord

extendNameType "NameType" [] [t|T|] defaultExtNameType

extendFunction "Function" [] [t|T|] defaultExtFunction

extendModule "Module" [] [t|T|] defaultExtModule

extendModuleE "ModuleE" [] [t|T|] defaultExtModuleE

extendFunctionLike "FunctionLike" [] [t|T|] $ const defaultExtFunctionLike

extendGuardBody "GuardBody" [] [t|T|] $ const defaultExtGuardBody

extendModuleOpen "ModuleOpen" [] [t|T|] defaultExtModuleOpen

extendModuleOpenExpr "ModuleOpenExpr" [] [t|T|] defaultExtModuleOpenExpr

extendArg "Arg" [] [t|T|] defaultExtArg

extendCond "Cond" [] [t|T|] $ const defaultExtCond

extendCondLogic "CondLogic" [] [t|T|] $ const defaultExtCondLogic

extendSignature "Signature" [] [t|T|] defaultExtSignature

extendExpression "Expression" [] [t|T|] defaultExtExpression

extendArrowExp "ArrowExp" [] [t|T|] defaultExtArrowExp

extendConstant "Constant" [] [t|T|] defaultExtConstant

extendNumb "Numb" [] [t|T|] defaultExtNumb

extendString' "String'" [] [t|T|] defaultExtString'

extendBlock "Block" [] [t|T|] defaultExtBlock

extendLambda "Lambda" [] [t|T|] defaultExtLambda

extendApplication "Application" [] [t|T|] defaultExtApplication

extendDo "Do" [] [t|T|] defaultExtDo

extendDoBody "DoBody" [] [t|T|] defaultExtDoBody

extendExpRecord "ExpRecord" [] [t|T|] defaultExtExpRecord

extendLet "Let" [] [t|T|] defaultExtLet

extendLetType "LetType" [] [t|T|] defaultExtLetType

extendInfix "Infix" [] [t|T|] defaultExtInfix

extendMatch "Match" [] [t|T|] defaultExtMatch

extendMatchL "MatchL" [] [t|T|] defaultExtMatchL

extendMatchLogic "MatchLogic" [] [t|T|] defaultExtMatchLogic

extendMatchLogicStart "MatchLogicStart" [] [t|T|] defaultExtMatchLogicStart

extendNameSet "NameSet" [] [t|T|] $ const defaultExtNameSet
