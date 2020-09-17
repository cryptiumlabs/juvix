module Juvix.Core.Erasure.Datatypes where

import qualified Data.HashMap.Strict as HM
import Juvix.Core.ErasedAnn.Conversion
import qualified Juvix.Core.ErasedAnn.Types as E
import Juvix.Core.Erasure.Types
import Juvix.Core.IR.Types (GlobalName, GlobalUsage (..))
import qualified Juvix.Core.Usage as Usage
import Juvix.Library hiding (Datatype)

inlineDatatypes :: Globals primTy primVal -> E.AnnTerm primTy primVal -> E.AnnTerm primTy primVal
inlineDatatypes globals ann@(E.Ann usage ty term) = do
  case term of
    E.Var sym ->
      case HM.lookup sym globals of
        Just (GDataCon con@(DataCon _ tye)) ->
          -- Turn ADT constructors into appropriate primitive constructors.
          let ty = convertType tye
              name = dataConTypeName ty
              ty' = inlineDatatypesInType globals ty
              Just (GDatatype d) = HM.lookup name globals
              cons = datatypeAndConToSimpleCons d con
           in E.Ann usage ty' (foldApp cons)
        Just (GFunction (Function _ _ tye clauses)) ->
          -- Turn functions into nested case trees.
          let ty = convertType tye
              ty' = inlineDatatypesInType globals ty
              (sym, caseTree) = clausesToCaseTree clauses
           in E.Ann usage ty' $ E.Case sym undefined caseTree
        Just (GAbstract u t) ->
          -- Inline abstract terms.
          convertTerm t (globalUsageToUsage u)
        -- Otherwise no action necessary.
        _ -> ann
    -- Recurse.
    E.LamM capture args body -> E.Ann usage ty $ E.LamM capture args (inlineDatatypes globals body)
    E.AppM f args -> E.Ann usage ty $ E.AppM f (map (inlineDatatypes globals) args)
    -- Else no action necessary.
    _ -> ann

clausesToCaseTree :: NonEmpty (FunClause primTy primVal) -> (Symbol, E.CaseTree primTy primVal)
clausesToCaseTree clauses = flattenedClausesToCaseTree $ flattenClauses clauses

flattenedClausesToCaseTree :: FlatClause primTy primVal -> (Symbol, E.CaseTree primTy primVal)
flattenedClausesToCaseTree flat =
  let go n f =
        let sym = depthToName n
         in case f of
              Terminal patts body -> (sym, pattsToCaseTree sym patts body)
              Nest patts nested ->
                (sym, pattsToCaseTree sym patts (let (sym, tree) = go (n + 1) nested in E.Ann undefined undefined $ E.Case sym undefined tree))
   in go 0 flat

pattsToCaseTree :: Symbol -> [Pattern primTy primVal] -> E.AnnTerm primTy primVal -> E.CaseTree primTy primVal
pattsToCaseTree sym patts body =
  case patts of
    -- Single pattern, must be tuple or var.
    patt : [] ->
      case patt of
        PVar v -> E.BindVar (depthToName v) body
        PCon _ [PVar l, PVar r] -> E.BindPair (depthToName l) (depthToName r) body
        _ -> undefined
    -- Two patterns, left / right.
    pattl : pattr : [] ->
      undefined
    -- Multiple patterns, multi-constructor.
    _ -> undefined

depthToName :: Int -> Symbol
depthToName = intern . show

flattenClauses :: NonEmpty (FunClause primTy primVal) -> FlatClause primTy primVal
flattenClauses = undefined

data FlatClause primTy primVal
  = Terminal [Pattern primTy primVal] (E.AnnTerm primTy primVal)
  | Nest [Pattern primTy primVal] (FlatClause primTy primVal)

globalUsageToUsage :: GlobalUsage -> Usage.T
globalUsageToUsage GZero = mempty
globalUsageToUsage GOmega = Usage.Omega

foldApp :: [E.Con] -> E.Term primTy primVal
foldApp = undefined

dataConTypeName :: E.Type primTy primVal -> GlobalName
dataConTypeName (E.SymT s) = s
dataConTypeName (E.Pi _ _ r) = dataConTypeName r

inlineDatatypesInType :: Globals primTy primVal -> E.Type primTy primVal -> E.Type primTy primVal
inlineDatatypesInType globals ty =
  case ty of
    E.SymT sym ->
      case HM.lookup sym globals of
        Just (GDatatype (Datatype _ _ _ cons)) ->
          let adt = consToADT globals cons in E.ADT adt
        _ -> ty
    _ -> ty

-- todo: find position for left/right, then calculate number of pairs
datatypeAndConToSimpleCons :: Datatype primTy -> DataCon primTy -> [E.Con]
datatypeAndConToSimpleCons = undefined

-- recurse and convert
consToADT :: Globals primTy primVal -> [DataCon primTy] -> E.ADT primTy
consToADT = undefined
