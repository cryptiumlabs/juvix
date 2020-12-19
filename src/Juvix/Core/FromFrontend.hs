{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Juvix.Core.FromFrontend where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Generics.SYB as SYB
import qualified Juvix.Core.Common.Context as Ctx
import qualified Juvix.Core.HR as HR
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.Types.Base as IR
import qualified Juvix.Core.Parameterisation as P
import Juvix.Core.Translate (hrToIR)
import qualified Juvix.FrontendContextualise as FE
import qualified Juvix.FrontendContextualise.InfixPrecedence.Types as FE
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Usage as Usage

data Error primTy primVal
  = -- features not yet implemented

    -- | constraints are not yet implemented
    ConstraintsUnimplemented [FE.Expression]
  | -- | refinements are not yet implemented
    RefinementsUnimplemented FE.TypeRefine
  | -- | universe polymorphism is not yet implemented
    UniversesUnimplemented FE.UniverseExpression
  | -- | implicit arguments are not yet implemented
    ImplicitsUnimplemented FE.ArrowExp
  | -- | implicit arguments are not yet implemented
    ImplicitsUnimplementedA FE.Arg
  | -- | type inference for definitions is not yet implemented
    SigRequired NameSymbol.T (FE.Final Ctx.Definition)
  | -- | head of application not an Elim
    NotAnElim FE.Expression
  | -- | pattern matching etc not yet implemented
    ExprUnimplemented FE.Expression
  | -- | local datatypes etc not yet implemented
    DefUnimplemented (FE.Final Ctx.Definition)
  | -- | patterns other than single vars in @let@ not yet implemented
    PatternUnimplemented FE.MatchLogic
  | -- | records not yet implemented
    RecordUnimplemented FE.Record
  | -- | records not yet implemented
    ExpRecordUnimplemented FE.ExpRecord
  | -- | records not yet implemented
    MatchRecordUnimplemented (NonEmpty (FE.NameSet FE.MatchLogic))
  | -- | lists not yet implemented
    ListUnimplemented FE.List
  | -- actual errors

    -- | unknown found at declaration level
    UnknownUnsupported (Maybe Symbol)
  | -- | current backend doesn't support this type of constant
    UnsupportedConstant FE.Constant
  | -- | current backend doesn't have this primitive
    UnknownPrimitive NameSymbol.T
  | -- | expression is not a usage
    NotAUsage FE.Expression
  | -- | expression is not 0 or ω
    NotAGUsage FE.Expression
  | -- | expression is not a natural number
    NotAUniverse FE.Expression
  | -- | usage is not 0 or ω
    UsageNotGUsage Usage.T
  | -- | invalid signature for declaration (bug in this module)
    -- @'Just' s@ if @s@ is a signature of the wrong shape,
    -- 'Nothing' if no signature found
    WrongSigType NameSymbol.T (Maybe (CoreSigHR primTy primVal))
  | -- | e.g. single anonymous constructor that is not a record
    IllFormedDatatype FE.Type
  | -- | e.g. ml-style constructor in a datatype with a GADT header
    InvalidConstructor NameSymbol.T FE.Product
  | -- | type is something other than a set of arrows ending in *
    InvalidDatatypeType (HR.Term primTy primVal)
  | -- | Not a special builtin (see 'Special')
    NotSpecial FE.Expression
  | -- | Unknown %Builtin.X
    UnknownBuiltin NameSymbol.T
  | -- | Builtin with usage
    BuiltinWithUsage (FE.Final Ctx.Definition)
  | -- | Builtin with type signature
    BuiltinWithTypeSig (FE.Final Ctx.Definition)
  | -- | Wrong number of arguments for a builtin
    WrongNumberBuiltinArgs Int [FE.Expression]
  | -- | Using omega as an expression
    UnexpectedOmega
  deriving (Show, Eq, Generic)

data CoreSig' ext primTy primVal
  = DataSig
      { dataType :: !(IR.Term' ext primTy primVal),
        -- | if declared as @type T a b = ...@, then the @T a b@ part
        -- (needed for ml-style constructors)
        dataHead :: !(Maybe (IR.Term' ext primTy primVal))
      }
  | ValSig
      { valUsage :: !IR.GlobalUsage,
        valType :: !(IR.Term' ext primTy primVal)
      }
  | SpecialSig !Special
  deriving (Generic)

-- | Bindings that can't be given types, but can be given new names by the user.
data Special
  = ArrowS (Maybe Usage.T) -- ^ pi type, possibly with usage already supplied
  | PairS (Maybe Usage.T)  -- ^ sigma type
  | ColonS                 -- ^ type annotation
  | TypeS                  -- ^ type of types
  | OmegaS                 -- ^ omega usage
  deriving (Eq, Show, Data, Generic)

deriving instance
  ( Eq primTy,
    Eq primVal,
    IR.TermAll Eq ext primTy primVal,
    IR.ElimAll Eq ext primTy primVal
  ) =>
  Eq (CoreSig' ext primTy primVal)

deriving instance
  ( Show primTy,
    Show primVal,
    IR.TermAll Show ext primTy primVal,
    IR.ElimAll Show ext primTy primVal
  ) =>
  Show (CoreSig' ext primTy primVal)

deriving instance
  ( Data ext,
    Data primTy,
    Data primVal,
    IR.TermAll Data ext primTy primVal,
    IR.ElimAll Data ext primTy primVal
  ) =>
  Data (CoreSig' ext primTy primVal)

type CoreSigIR = CoreSig' IR.NoExt

type CoreSigHR = CoreSig' HR.T

type CoreSigs' ext primTy primVal =
  HashMap IR.GlobalName (CoreSig' ext primTy primVal)

type CoreSigsIR primTy primVal = CoreSigs' IR.NoExt primTy primVal

type CoreSigsHR primTy primVal = CoreSigs' HR.T primTy primVal

data CoreDef primTy primVal
  = CoreDef !(IR.RawGlobal primTy primVal)
  | SpecialDef !NameSymbol.T !Special
  deriving (Eq, Show, Data, Generic)

type CoreDefs primTy primVal = HashMap IR.GlobalName (CoreDef primTy primVal)

data FFState primTy primVal
  = FFState
      { frontend :: FE.FinalContext,
        param :: P.Parameterisation primTy primVal,
        coreSigs :: CoreSigsHR primTy primVal,
        core :: CoreDefs primTy primVal,
        patVars :: HashMap IR.GlobalName IR.PatternVar,
        nextPatVar :: IR.PatternVar
      }
  deriving (Generic)

type EnvAlias primTy primVal =
  ExceptT (Error primTy primVal) (State (FFState primTy primVal))

newtype Env primTy primVal a
  = Env {unEnv :: EnvAlias primTy primVal a}
  deriving newtype (Functor, Applicative, Monad)
  deriving
    (HasThrow "fromFrontendError" (Error primTy primVal))
    via MonadError (EnvAlias primTy primVal)
  deriving
    ( HasSource "frontend" FE.FinalContext,
      HasReader "frontend" FE.FinalContext
    )
    via ReaderField "frontend" (EnvAlias primTy primVal)
  deriving
    ( HasSource "param" (P.Parameterisation primTy primVal),
      HasReader "param" (P.Parameterisation primTy primVal)
    )
    via ReaderField "param" (EnvAlias primTy primVal)
  deriving
    ( HasSource "coreSigs" (CoreSigsHR primTy primVal),
      HasSink "coreSigs" (CoreSigsHR primTy primVal),
      HasState "coreSigs" (CoreSigsHR primTy primVal)
    )
    via StateField "coreSigs" (EnvAlias primTy primVal)
  deriving
    ( HasSource "core" (CoreDefs primTy primVal),
      HasSink "core" (CoreDefs primTy primVal),
      HasState "core" (CoreDefs primTy primVal)
    )
    via StateField "core" (EnvAlias primTy primVal)
  deriving
    ( HasSource "patVars" (HashMap IR.GlobalName IR.PatternVar),
      HasSink "patVars" (HashMap IR.GlobalName IR.PatternVar),
      HasState "patVars" (HashMap IR.GlobalName IR.PatternVar)
    )
    via StateField "patVars" (EnvAlias primTy primVal)
  deriving
    ( HasSource "nextPatVar" IR.PatternVar,
      HasSink "nextPatVar" IR.PatternVar,
      HasState "nextPatVar" IR.PatternVar
    )
    via StateField "nextPatVar" (EnvAlias primTy primVal)

execEnv ::
  FE.FinalContext ->
  P.Parameterisation primTy primVal ->
  Env primTy primVal a ->
  Either (Error primTy primVal) a
execEnv ctx param (Env env) =
  fst $ runIdentity $ runStateT (runExceptT env) initState
  where
    initState =
      FFState
        { frontend = ctx,
          param,
          coreSigs = mempty,
          core = mempty,
          patVars = mempty,
          nextPatVar = 0
        }

throwFF :: Error primTy primVal -> Env primTy primVal a
throwFF = throw @"fromFrontendError"

paramConstant' ::
  P.Parameterisation primTy primVal ->
  FE.Constant ->
  Maybe primVal
paramConstant' p (FE.Number (FE.Double' d)) = P.floatVal p d
paramConstant' p (FE.Number (FE.Integer' i)) = P.intVal p i
paramConstant' p (FE.String (FE.Sho s)) = P.stringVal p s

paramConstant :: FE.Constant -> Env primTy primVal primVal
paramConstant k = do
  p <- ask @"param"
  case paramConstant' p k of
    Just x -> pure x
    Nothing -> throwFF $ UnsupportedConstant k

transformTermIR ::
  (Data primTy, Data primVal) =>
  FE.Expression ->
  Env primTy primVal (IR.Term primTy primVal)
transformTermIR fe = do
  SYB.everywhereM (SYB.mkM transformPatVar) . hrToIR =<< transformTermHR fe

transformPatVar :: IR.Name -> Env primTy primVal IR.Name
transformPatVar (IR.Global name) =
  gets @"patVars" $
    maybe (IR.Global name) IR.Pattern
      . HM.lookup name
transformPatVar p = pure p

-- | N.B. doesn't deal with pattern variables since HR doesn't have them.
-- 'transformTermIR' does that.
transformTermHR :: FE.Expression -> Env primTy primVal (HR.Term primTy primVal)
transformTermHR (FE.Constant k) = HR.Prim <$> paramConstant k
transformTermHR (FE.Let l) = transformSimpleLet l
transformTermHR e@(FE.LetType _) = throwFF $ ExprUnimplemented e
transformTermHR e@(FE.Match _) = throwFF $ ExprUnimplemented e
transformTermHR (FE.Name n) = pure $ HR.Elim $ HR.Var n
transformTermHR (FE.Lambda l) = transformSimpleLambda l
transformTermHR (FE.Application app) = transformApplication app
transformTermHR (FE.Primitive (FE.Prim p)) = do
  param <- ask @"param"
  maybe (throwFF $ UnknownPrimitive p) pure $
    primTy param p <|> primVal param p
  where
    primTy param p = HR.PrimTy <$> HM.lookup p (P.builtinTypes param)
    primVal param p = HR.Prim <$> HM.lookup p (P.builtinValues param)
transformTermHR (FE.List l) =
  throwFF $ ListUnimplemented l
transformTermHR (FE.Tuple (FE.TupleLit es)) =
  makeTuple <$> traverse transformTermHR es
transformTermHR (FE.Block (FE.Bloc e)) =
  transformTermHR e
transformTermHR (FE.ExpRecord r) =
  throwFF $ ExpRecordUnimplemented r
transformTermHR (FE.ArrowE a) = transformArrow a
transformTermHR (FE.NamedTypeE (FE.NamedType' _ e)) =
  -- TODO the name will only become relevant (outside of arrows)
  -- when refinements are supported
  transformTermHR e
transformTermHR (FE.RefinedE r) =
  throwFF $ RefinementsUnimplemented r
transformTermHR (FE.UniverseName i) =
  -- TODO for universe polymorphism
  throwFF $ UniversesUnimplemented i
transformTermHR (FE.Parened e) = transformTermHR e
transformTermHR (FE.DeclarationE (FE.DeclareExpression _ e)) =
  transformTermHR e

transformApplication ::
  FE.Application ->
  Env primTy primVal (HR.Term primTy primVal)
transformApplication (FE.App f xs) = getSpecial f >>= flip go (toList xs) where
  go s xs = case s of
    Just (ArrowS Nothing) -> do
      ~[π, a, b] <- nargs 3 xs
      π <- transformUsage π
      go (Just (ArrowS (Just π))) [a, b]
    Just (ArrowS (Just π)) -> do
      ~[xa, b] <- nargs 2 xs
      let (x, a) = namedArg xa
      HR.Pi π x <$> transformTermHR a <*> transformTermHR b
    Just (PairS Nothing) -> do
      ~[π, a, b] <- nargs 3 xs
      π <- transformUsage π
      go (Just (PairS (Just π))) [a, b]
    Just (PairS (Just π)) -> do
      ~[xa, b] <- nargs 2 xs
      let (x, a) = namedArg xa
      HR.Sig π x <$> transformTermHR a <*> transformTermHR b
    Just ColonS -> do
      ~[a, b] <- nargs 2 xs
      a <- transformTermHR a
      b <- transformTermHR b
      -- FIXME metavars for usage & universe
      pure $ HR.Elim $ HR.Ann (Usage.SNat 1) a b 0
    Just TypeS -> do
      ~[i] <- nargs 1 xs
      HR.Star <$> transformUniverse i
    Just OmegaS ->
      throwFF UnexpectedOmega
    Nothing -> do
      f' <- toElim f =<< transformTermHR f
      HR.Elim . foldl HR.App f' <$> traverse transformTermHR xs
  nargs n xs
    | (xs, []) <- splitAt n (toList xs),
      length xs == n
    = pure xs
    | otherwise
    = throwFF $ WrongNumberBuiltinArgs n xs
  namedArg (FE.NamedTypeE (FE.NamedType' (FE.Concrete x) e)) =
    (NameSymbol.fromSymbol x, e)
  -- TODO implicit arguments???
  namedArg e = ("" :| [], e)

transformSimpleLet :: FE.Let -> Env primTy primVal (HR.Term primTy primVal)
transformSimpleLet e@(FE.LetGroup name (clause :| []) body) = do
  let FE.Like args cbody = clause
  rhs <-
    toElim (FE.Let e)
      =<< foldr HR.Lam <$> transformTermHR cbody <*> traverse isVarArg args
  HR.Let Usage.Omega (NameSymbol.fromSymbol name) rhs <$> transformTermHR body
-- FIXME: usage in frontend let
transformSimpleLet e = throwFF $ ExprUnimplemented (FE.Let e)

transformSimpleLambda :: FE.Lambda -> Env primTy primVal (HR.Term primTy primVal)
transformSimpleLambda (FE.Lamb pats body) = do
  foldr HR.Lam <$> transformTermHR body <*> traverse isVarPat pats

isVarArg :: FE.Arg -> Env primTy primVal NameSymbol.T
isVarArg (FE.ConcreteA p) = isVarPat p
isVarArg a = throwFF $ ImplicitsUnimplementedA a

isVarPat :: FE.MatchLogic -> Env primTy primVal NameSymbol.T
isVarPat (FE.MatchLogic (FE.MatchName x) Nothing) =
  -- FIXME is a nullary constructor expressed as
  -- @MatchCon n []@ or just as @MatchName n@?
  pure $ NameSymbol.fromSymbol x
isVarPat p = throwFF $ PatternUnimplemented p

transformArrow :: FE.ArrowExp -> Env primTy primVal (HR.Term primTy primVal)
transformArrow f@(FE.Arr' xa π b) =
  case xa of
    FE.NamedTypeE (FE.NamedType' x a) -> go π (getName x) a b
    a -> go π (pure "") a b
  where
    getName (FE.Concrete x) = pure $ NameSymbol.fromSymbol x
    getName (FE.Implicit _) = throwFF $ ImplicitsUnimplemented f
    go π x a b =
      HR.Pi <$> transformUsage π
        <*> x
        <*> transformTermHR a
        <*> transformTermHR b

-- | translate (1,2,3,4) to (1,(2,(3,4)))
makeTuple :: [HR.Term primTy primVal] -> HR.Term primTy primVal
makeTuple [] = HR.Unit
makeTuple [t] = t
makeTuple (t : ts) = HR.Pair t (makeTuple ts)

-- TODO for records:
-- 1) an extended version of core with record literals to begin with
-- 2) translation after typechecking of these to an application of the right
--    constructor to the fields in the originally declared order

toElim ::
  -- | the original expression
  FE.Expression ->
  HR.Term primTy primVal ->
  Env primTy primVal (HR.Elim primTy primVal)
toElim _ (HR.Elim e) = pure e
toElim e _ = throwFF $ NotAnElim e -- FIXME add metavar ann

-- TODO put an annotation with metas for the usage/type

isOmega :: FE.Expression -> Env primTy primVal Bool
isOmega e = do
  s <- getSpecial e
  pure case s of
    Just OmegaS -> True
    _           -> False

pattern FEIntLit :: Integer -> FE.Expression
pattern FEIntLit i = FE.Constant (FE.Number (FE.Integer' i))

transformUsage :: FE.Expression -> Env primTy primVal Usage.T
transformUsage (FEIntLit i) | i >= 0 = pure $ Usage.SNat $ fromInteger i
transformUsage e = do
  o <- isOmega e
  if o then pure Usage.Omega else throwFF $ NotAUsage e

transformGUsage :: Maybe FE.Expression -> Env primTy primVal IR.GlobalUsage
transformGUsage Nothing = pure IR.GOmega
transformGUsage (Just (FEIntLit 0)) = pure IR.GZero
transformGUsage (Just e) = do
  o <- isOmega e
  if o then pure IR.GOmega else throwFF $ NotAGUsage e

transformUniverse :: FE.Expression -> Env primTy primVal IR.Universe
transformUniverse (FEIntLit i) | i >= 0 = pure $ fromIntegral i
transformUniverse e = throwFF $ NotAUniverse e


getSpecial ::
  FE.Expression ->
  Env primTy primVal (Maybe Special)
getSpecial (FE.Name x) = do
  sig <- getSig' Just x
  pure case sig of
    SpecialSig s -> Just s
    _            -> Nothing
getSpecial _ = pure Nothing

transformSpecialRhs ::
  FE.Expression ->
  Env primTy primVal (Maybe Special)
transformSpecialRhs (FE.Primitive (FE.Prim p)) =
  case p of
    "Builtin" :| ["Arrow"] -> pure $ Just $ ArrowS Nothing
    "Builtin" :| ["Pair"]  -> pure $ Just $ PairS Nothing
    "Builtin" :| ["Omega"] -> pure $ Just OmegaS
    "Builtin" :| ["Colon"] -> pure $ Just ColonS
    "Builtin" :| ["Type"]  -> pure $ Just TypeS
    "Builtin" :| (s:ss)    -> throwFF $ UnknownBuiltin $ s :| ss
    _                      -> pure Nothing
transformSpecialRhs (FE.Application (FE.App f (arg :| []))) = do
  head <- getSpecial f
  case head of
    Nothing               -> pure Nothing
    Just (ArrowS Nothing) -> Just . ArrowS . Just <$> transformUsage arg
    Just (PairS  Nothing) -> Just . PairS  . Just <$> transformUsage arg
    Just _                ->
      -- an error, but 'transformNormal' will catch it
      pure Nothing
transformSpecialRhs _ = pure Nothing

transformSpecial ::
  FE.Final Ctx.Definition ->
  Env primTy primVal (Maybe Special)
transformSpecial def@(Ctx.Def π ty (FE.Like [] rhs :| []) _) = do
  rhs <- transformSpecialRhs rhs
  when (isJust rhs) do
    unless (isNothing π) $ throwFF $ BuiltinWithUsage def
    unless (isNothing ty) $ throwFF $ BuiltinWithTypeSig def
  pure rhs
transformSpecial _ = pure Nothing

transformSig ::
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  Env primTy primVal (Maybe (CoreSigHR primTy primVal))
transformSig x def = trySpecial <||> tryNormal where
  trySpecial = fmap SpecialSig <$> transformSpecial def
  tryNormal  = transformNormalSig x def
  x <||> y   = x >>= maybe y (pure . Just)

transformNormalSig ::
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  Env primTy primVal (Maybe (CoreSigHR primTy primVal))
transformNormalSig x def@(Ctx.Def π msig _ _) =
  Just <$> transformValSig x def π msig
transformNormalSig _ (Ctx.Record _ _) = pure Nothing -- TODO
transformNormalSig x (Ctx.TypeDeclar typ) = Just <$> transformTypeSig x typ
transformNormalSig _ (Ctx.Unknown sig) =
  throwFF $ UnknownUnsupported $ FE.signatureName <$> sig
transformNormalSig _ Ctx.CurrentNameSpace = pure Nothing
transformNormalSig _ (Ctx.Information {}) = pure Nothing

transformTypeSig ::
  NameSymbol.T ->
  FE.Type ->
  Env primTy primVal (CoreSigHR primTy primVal)
transformTypeSig name (FE.Typ {typeArgs, typeForm}) = do
  baseTy <- case typeForm of
    FE.Arrowed {dataArrow} -> transformTermHR dataArrow
    FE.NonArrowed {} -> pure $ HR.Star 0 -- TODO metavar (level might not be 0)
  pure $
    DataSig
      { dataType = foldr makeTPi baseTy typeArgs,
        dataHead = case typeForm of
          FE.Arrowed {} -> Nothing
          FE.NonArrowed {} -> Just $ HR.Elim $ foldl HR.App hd args
            where
              hd = HR.Var name
              args = HR.Elim . HR.Var . NameSymbol.fromSymbol <$> typeArgs
      }
  where
    makeTPi name res =
      -- TODO metavars for the named args instead of defaulting to types
      HR.Pi mempty (NameSymbol.fromSymbol name) (HR.Star 0) res

transformValSig ::
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  Maybe Usage.T ->
  Maybe FE.Signature ->
  Env primTy primVal (CoreSigHR primTy primVal)
transformValSig _ _ _ (Just (FE.Sig _ π ty cons))
  | null cons = ValSig <$> transformGUsage π <*> transformTermHR ty
  | otherwise = throwFF $ ConstraintsUnimplemented cons
transformValSig x def _ _ = throwFF $ SigRequired x def

transformDef ::
  (Data primTy, Data primVal) =>
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  Env primTy primVal [CoreDef primTy primVal]
transformDef x def = do
  sig <- getSig' Just x
  case sig of
    SpecialSig s -> pure [SpecialDef x s]
    _ -> map CoreDef <$> transformNormalDef x def

transformNormalDef ::
  (Data primTy, Data primVal) =>
  NameSymbol.T ->
  FE.Final Ctx.Definition ->
  Env primTy primVal [IR.RawGlobal primTy primVal]
transformNormalDef x (Ctx.Def _ _ def _) = do
  (π, typ) <- getValSig x
  clauses <- traverse transformClause def
  let f =
        IR.Function
          { funName = x,
            funUsage = π,
            funType = hrToIR typ,
            funClauses = clauses
          }
  pure [IR.GFunction f]
transformNormalDef _ (Ctx.Record _ _) = pure [] -- TODO
transformNormalDef x (Ctx.TypeDeclar dec) = transformType x dec
transformNormalDef _ (Ctx.Unknown _) = pure []
transformNormalDef _ Ctx.CurrentNameSpace = pure []
transformNormalDef _ (Ctx.Information {}) = pure []

getValSig ::
  NameSymbol.T ->
  Env primTy primVal (IR.GlobalUsage, HR.Term primTy primVal)
getValSig = getSig' \case ValSig π ty -> Just (π, ty); _ -> Nothing

getDataSig ::
  NameSymbol.T ->
  Env primTy primVal (HR.Term primTy primVal, Maybe (HR.Term primTy primVal))
getDataSig = getSig' \case DataSig ty hd -> Just (ty, hd); _ -> Nothing

getSig' ::
  (CoreSigHR primTy primVal -> Maybe a) -> NameSymbol.T -> Env primTy primVal a
getSig' f x = do
  msig <- gets @"coreSigs" $ HM.lookup x
  case msig of
    Just sig | Just ty <- f sig -> pure ty
    _ -> throwFF $ WrongSigType x msig

transformType ::
  (Data primTy, Data primVal) =>
  NameSymbol.T ->
  FE.Type ->
  Env primTy primVal [IR.RawGlobal primTy primVal]
transformType name dat@(FE.Typ {typeForm}) = do
  (ty, hdHR) <- getDataSig name
  let hd = hrToIR <$> hdHR
  case body of
    FE.Product (FE.Record r) -> throwFF $ RecordUnimplemented r
    FE.Product _ -> throwFF $ IllFormedDatatype dat
    FE.Sum cons -> do
      let qual = NameSymbol.modName name
      (args, level) <- splitDataType ty
      cons <- traverse (transformCon qual hd) $ toList cons
      let dat' =
            IR.Datatype
              { dataName = name,
                dataArgs = args,
                dataLevel = level,
                dataCons = cons
              }
      pure $ IR.GDatatype dat' : fmap IR.GDataCon cons
  where
    body = case typeForm of
      FE.Arrowed {dataAdt' = b} -> b
      FE.NonArrowed {dataAdt = b} -> b

splitDataType ::
  HR.Term primTy primVal ->
  Env primTy primVal ([IR.RawDataArg primTy primVal], IR.Universe)
splitDataType ty0 = go ty0
  where
    go (HR.Pi π x s t) = first (arg :) <$> splitDataType t
      where
        arg =
          IR.DataArg
            { argName = x,
              argUsage = π,
              argType = hrToIR s,
              argIsParam = False -- TODO parameter detection
            }
    go (HR.Star ℓ) = pure ([], ℓ)
    go _ = throwFF $ InvalidDatatypeType ty0

transformCon ::
  (Data primTy, Data primVal) =>
  [Symbol] ->
  Maybe (IR.Term primTy primVal) ->
  FE.Sum ->
  Env primTy primVal (IR.RawDataCon primTy primVal)
transformCon qual hd (FE.S name prod) =
  transformCon' (NameSymbol.qualify1 qual name) hd $
    fromMaybe (FE.ADTLike []) prod

transformCon' ::
  (Data primTy, Data primVal) =>
  NameSymbol.T ->
  Maybe (IR.Term primTy primVal) ->
  FE.Product ->
  Env primTy primVal (IR.RawDataCon primTy primVal)
transformCon' _ _ (FE.Record r) = throwFF $ RecordUnimplemented r
transformCon' name _ (FE.Arrow ty) = IR.DataCon name <$> transformTermIR ty
transformCon' name Nothing k@(FE.ADTLike {}) =
  throwFF $ InvalidConstructor name k
transformCon' name (Just hd) (FE.ADTLike tys) =
  IR.DataCon name <$> foldrM makeArr hd tys
  where
    makeArr arg res = IR.Pi (Usage.SNat 1) <$> transformTermIR arg <*> pure res

transformClause ::
  (Data primTy, Data primVal) =>
  FE.FunctionLike FE.Expression ->
  Env primTy primVal (IR.FunClause primTy primVal)
transformClause (FE.Like args body) = do
  put @"patVars" mempty
  put @"nextPatVar" 0
  IR.FunClause <$> traverse transformArg args <*> transformTermIR body

transformArg :: FE.Arg -> Env primTy primVal (IR.Pattern primTy primVal)
transformArg a@(FE.ImplicitA _) = throwFF $ ImplicitsUnimplementedA a
transformArg (FE.ConcreteA pat) = transformPat pat

transformPat :: FE.MatchLogic -> Env primTy primVal (IR.Pattern primTy primVal)
transformPat (FE.MatchLogic pat _) = case pat of
  -- TODO translate as-patterns into @let@
  FE.MatchCon k pats -> IR.PCon k <$> traverse transformPat pats
  FE.MatchName x -> do
    var <- getNextPatVar
    modify @"patVars" $ HM.insert (NameSymbol.fromSymbol x) var
    pure $ IR.PVar var
  FE.MatchConst p ->
    IR.PPrim <$> paramConstant p
  FE.MatchRecord r ->
    throwFF $ MatchRecordUnimplemented r

getNextPatVar :: Env primTy primVal IR.PatternVar
getNextPatVar = state @"nextPatVar" \v -> (v, succ v)
