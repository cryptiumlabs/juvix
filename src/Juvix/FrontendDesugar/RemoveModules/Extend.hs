module Juvix.FrontendDesugar.RemoveModules.Extend where

import Juvix.Frontend.Types.Base
import Juvix.Library hiding (Product, Sum)

extendType :: ExtType
extendType = defaultExtType

extendTopLevel :: ExtTopLevel
extendTopLevel = defaultExtTopLevel {typeModule = Nothing}

extendTypeSum :: ExtTypeSum
extendTypeSum = defaultExtTypeSum

extendData :: ExtData
extendData = defaultExtData

extendAlias :: ExtAlias
extendAlias = defaultExtAlias

extendName :: ExtName
extendName = defaultExtName

extendNamedType :: ExtNamedType
extendNamedType = defaultExtNamedType

extendTypeRefine :: ExtTypeRefine
extendTypeRefine = defaultExtTypeRefine

extendArrowSymbol :: ExtArrowSymbol
extendArrowSymbol = defaultExtArrowSymbol

extendUniverseExpression :: ExtUniverseExpression
extendUniverseExpression = defaultExtUniverseExpression

extendAdt :: ExtAdt
extendAdt = defaultExtAdt

extendSum :: ExtSum
extendSum = defaultExtSum

extendProduct :: ExtProduct
extendProduct = defaultExtProduct

extendRecord :: ExtRecord
extendRecord = defaultExtRecord

extendNameType :: ExtNameType
extendNameType = defaultExtNameType

extendFunction :: ExtFunction
extendFunction = defaultExtFunction

extendArg :: ExtArg
extendArg = defaultExtArg

extendFunctionLike :: ExtFunctionLike
extendFunctionLike = defaultExtFunctionLike

extendModuleOpen :: ExtModuleOpen
extendModuleOpen = defaultExtModuleOpen

extendModuleOpenExpr :: ExtModuleOpenExpr
extendModuleOpenExpr = defaultExtModuleOpenExpr

extendSignature :: ExtSignature
extendSignature = defaultExtSignature

extendExpression :: ExtExpression
extendExpression = defaultExtExpression {typeModuleE = Nothing}

extendArrowExp :: ExtArrowExp
extendArrowExp = defaultExtArrowExp

extendCond :: ExtCond
extendCond = defaultExtCond

extendCondLogic :: ExtCondLogic
extendCondLogic = defaultExtCondLogic

extendConstant :: ExtConstant
extendConstant = defaultExtConstant

extendNumb :: ExtNumb
extendNumb = defaultExtNumb

extendString' :: ExtString'
extendString' = defaultExtString'

extendBlock :: ExtBlock
extendBlock = defaultExtBlock

extendLambda :: ExtLambda
extendLambda = defaultExtLambda

extendApplication :: ExtApplication
extendApplication = defaultExtApplication

extendDo :: ExtDo
extendDo = defaultExtDo

extendDoBody :: ExtDoBody
extendDoBody = defaultExtDoBody

extendExpRecord :: ExtExpRecord
extendExpRecord = defaultExtExpRecord

extendLet :: ExtLet
extendLet = defaultExtLet

extendLetType :: ExtLetType
extendLetType = defaultExtLetType

extendInfix :: ExtInfix
extendInfix = defaultExtInfix

extendMatch :: ExtMatch
extendMatch = defaultExtMatch

extendMatchL :: ExtMatchL
extendMatchL = defaultExtMatchL

extendMatchLogic :: ExtMatchLogic
extendMatchLogic = defaultExtMatchLogic

extendMatchLogicStart :: ExtMatchLogicStart
extendMatchLogicStart = defaultExtMatchLogicStart

extendNameSet :: ExtNameSet
extendNameSet = defaultExtNameSet
