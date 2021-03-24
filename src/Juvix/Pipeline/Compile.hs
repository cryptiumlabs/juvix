{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.Compile where

import Data.Field.Galois (GaloisField)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Juvix.Backends.Michelson.Compilation as M
import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Backends.Plonk as Plonk
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.ErasedAnn as ErasedAnn
import qualified Juvix.Core.IR.Typechecker.Types as TypeChecker
import Juvix.Core.FromFrontend as FF
import qualified Juvix.Core.IR as IR
import qualified Juvix.Core.IR.TransformExt.OnlyExts as OnlyExts
import Juvix.Core.IR.Types.Base
import Juvix.Core.IR.Types.Globals
import Juvix.Core.Parameterisation
import qualified Juvix.Core.Pipeline as CorePipeline
import qualified Juvix.FrontendContextualise.InfixPrecedence.Environment as FE
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Pipeline.Internal as Pipeline
import Juvix.Pipeline.Types
import qualified System.IO.Temp as Temp
import qualified Prelude as P
import qualified Text.PrettyPrint.Leijen.Text as Pretty

type Code = Text

type OutputCode = Text

type Message = P.String

type Pipeline = Feedback.FeedbackT [] Message IO

class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b

  typecheck :: FE.FinalContext -> Pipeline (ErasedAnn.AnnTerm (Ty b) (CoreApp.Return' ErasedAnn.T (NonEmpty (Ty b)) (Val b)))
  compile :: ErasedAnn.AnnTerm (Ty b) (CoreApp.Return' ErasedAnn.T (NonEmpty (Ty b)) (Val b)) -> Pipeline Text

data BMichelson = BMichelson
  deriving (Eq, Show)

instance HasBackend BMichelson where
  type Ty BMichelson = Param.PrimTy
  type Val BMichelson = Param.RawPrimVal
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx Param.michelson
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe toCoreDef globals
        case HM.elems $ HM.filter isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (convGlobal Param.Set) globalDefs
                  newGlobals = HM.map (unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ exec (CorePipeline.coreToAnn inlinedTerm (IR.globalToUsage usage) ty) Param.michelson newGlobals
              case res of
                Right r -> do
                  pure r
                Left err -> do
                  print term
                  Feedback.fail $ show err
          somethingElse -> do
            Feedback.fail $ show somethingElse
      Left err -> do
        Feedback.fail $ "failed at ctxToCore\n" ++ show err
  typecheck _ =
    Feedback.fail $ "Typecheck not implemented for Michelson backend."

  compile term = do
    let (res, _logs) = M.compileContract $ CorePipeline.toRaw term
    case res of
      Right c -> do
        return $ M.untypedContractToSource (fst c)
      Left err -> Feedback.fail $ show err

data BPlonk f = BPlonk
  deriving (Eq, Show)

instance
  ( GaloisField f,
    Data f,
    Eq f,
    Integral f,
    Data (Plonk.PrimTy f),
    Eq (Plonk.PrimTy f),
    Show (Plonk.PrimTy f),
    CanApply
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Plonk.PrimTy f))
          (Plonk.PrimVal f)
      ),
    CanApply (Plonk.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Plonk.PrimTy f)
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Plonk.PrimTy f))
          (Plonk.PrimVal f)
      )
      (Plonk.PrimTy f),
    IR.HasWeak (Plonk.PrimVal f),
    IR.HasSubstValue
      IR.NoExt
      (Plonk.PrimTy f)
      ( CoreApp.Return'
          IR.NoExt
          (NonEmpty (Plonk.PrimTy f))
          (Plonk.PrimVal f)
      )
      (Plonk.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Plonk.PrimTy f)
      (Plonk.PrimVal f)
      (Plonk.PrimTy f),
    IR.HasPatSubstTerm
      (OnlyExts.T IR.NoExt)
      (Plonk.PrimTy f)
      (Plonk.PrimVal f)
      (Plonk.PrimVal f),
    IR.HasPatSubstTerm
                          (OnlyExts.T TypeChecker.T)
                          (Plonk.PrimTy f)
                          (TypedPrim (Plonk.PrimTy f) (Plonk.PrimVal f))
                          (Plonk.PrimTy f),
    Show (Arg (Plonk.PrimTy f)),
    Show
                          (Arg (TypedPrim (Plonk.PrimTy f) (Plonk.PrimVal f))),
    Show (ApplyErrorExtra (Plonk.PrimTy f)),
    Show
                          (ApplyErrorExtra (TypedPrim (Plonk.PrimTy f) (Plonk.PrimVal f)))
  ) =>
  HasBackend (BPlonk f)
  where
  type Ty (BPlonk f) = Plonk.PrimTy f
  type Val (BPlonk f) = Plonk.PrimVal f
  typecheck ctx = do
    let res = Pipeline.contextToCore ctx (Plonk.plonk @f)
    case res of
      Right (FF.CoreDefs _order globals) -> do
        let globalDefs = HM.mapMaybe toCoreDef globals
        case HM.elems $ HM.filter isMain globalDefs of
          [] -> Feedback.fail "No main function found"
          [IR.RawGFunction f]
            | IR.RawFunction _name usage ty (clause :| []) <- f,
              IR.RawFunClause _ [] term _ <- clause -> do
              let convGlobals = map (convGlobal Plonk.PrimTy) globalDefs
                  newGlobals = HM.map (unsafeEvalGlobal convGlobals) convGlobals
                  lookupGlobal = IR.rawLookupFun' globalDefs
                  inlinedTerm = IR.inlineAllGlobals term lookupGlobal
              (res, _) <- liftIO $ exec (CorePipeline.coreToAnn inlinedTerm (IR.globalToUsage usage) ty) (Plonk.plonk @f) newGlobals
              -- notImplemented 
              -- (res, _) <- notImplemented -- liftIO $ exec (CorePipeline.coreToAnn inlinedTerm (IR.globalToUsage usage) ty) (Plonk.plonk @f) newGlobals
              case res of
                Right r -> do
                  pure r
                Left err -> do
                  print term
                  Feedback.fail $ show err
          somethingElse ->  Feedback.fail $ show somethingElse
      Left err -> Feedback.fail $ "failed at ctxToCore\n" ++ show err
  typecheck _ =
    Feedback.fail $ "Typecheck not implemented for Plonk backend."

  -- TODO: Serialize circuit
  -- TODO: Homogenize pretty printers!
  compile term = 
    pure . toS . Pretty.displayT . Pretty.renderPretty 1 120 . Pretty.pretty . Plonk.execCircuitBuilder  . Plonk.compileTermWithWire $  CorePipeline.toRaw term

parse :: (MonadIO m, P.MonadFail m) => Code -> m FE.FinalContext
parse code = do
  core <- liftIO $ toCore_wrap code
  case core of
    Right ctx -> return ctx
    Left err -> Feedback.fail $ show err
  where
    toCore_wrap :: Code -> IO (Either Pipeline.Error FE.FinalContext)
    toCore_wrap code = do
      fp <- Temp.writeSystemTempFile "juvix-toCore.ju" (Text.unpack code)
      Pipeline.toCore
        [ "stdlib/Prelude.ju",
          "stdlib/Michelson.ju",
          "stdlib/MichelsonAlias.ju",
          fp
        ]

toCoreDef ::
  Alternative f =>
  CoreDef primTy primVal ->
  f (IR.RawGlobal primTy primVal)
toCoreDef (CoreDef g) = pure g
toCoreDef _ = empty

isMain :: RawGlobal' ext primTy primVal -> Bool
isMain (IR.RawGFunction (IR.RawFunction (_ :| ["main"]) _ _ _)) = True
isMain _ = False

-- | Write the output code to a given file.
writeout :: FilePath -> OutputCode -> Pipeline ()
writeout fout code = liftIO $ T.writeFile fout code

unsafeEvalGlobal ::
  IR.CanEval IR.NoExt IR.NoExt primTy primVal =>
  IR.RawGlobals primTy primVal ->
  IR.RawGlobal primTy primVal ->
  IR.Global primTy primVal
unsafeEvalGlobal globals g =
  case g of
    RawGDatatype (RawDatatype n pos a l cons) -> undefined
    RawGDataCon (RawDataCon n t d) -> undefined
    RawGFunction (RawFunction n u t cs) ->
      GFunction $
        Function n u (unsafeEval globals t) (map (funClauseEval globals) cs)
    RawGAbstract (RawAbstract n u t) ->
      GAbstract $ Abstract n u (unsafeEval globals t)

convGlobal ::
  ty ->
  IR.RawGlobal ty val ->
  IR.RawGlobal ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
convGlobal ty g =
  case g of
    RawGDatatype (RawDatatype n pos a l cons) -> undefined
    RawGDataCon (RawDataCon n t d) -> undefined
    RawGFunction (RawFunction n u t cs) ->
      RawGFunction (RawFunction n u (baseToReturn ty t) (funClauseReturn ty <$> cs))
    RawGAbstract (RawAbstract n u t) ->
      RawGAbstract (RawAbstract n u (baseToReturn ty t))

funClauseReturn ::
  ty ->
  IR.RawFunClause ty val ->
  IR.RawFunClause ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val)
funClauseReturn ty (RawFunClause _tel patts term _catchall) =
  RawFunClause undefined (map (pattEval ty) patts) (baseToReturn ty term) undefined

-- TODO

funClauseEval ::
  IR.RawGlobals primTy primVal ->
  IR.RawFunClause primTy primVal ->
  IR.FunClause primTy primVal
funClauseEval globals (RawFunClause _tel patts rhs _catchall) =
  FunClause undefined patts rhs undefined undefined undefined --TODO

pattEval ::
  ty ->
  IR.Pattern ty val ->
  IR.Pattern ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val) -- ty' -- Param.PrimValIR
pattEval ty patt =
  case patt of
    IR.PCon n ps -> IR.PCon n (map (pattEval ty) ps)
    IR.PPair x y -> IR.PPair (pattEval ty x) (pattEval ty y)
    IR.PUnit -> IR.PUnit
    IR.PVar v -> IR.PVar v
    IR.PDot t -> IR.PDot (baseToReturn ty t)
    -- TODO
    IR.PPrim p -> IR.PPrim (CoreApp.Return (ty :| []) p)

baseToReturn ::
  ty ->
  Term' IR.NoExt ty val ->
  Term' IR.NoExt ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val) -- ty' -- Param.PrimValIR
baseToReturn ty t =
  case t of
    IR.Star u -> IR.Star u
    IR.PrimTy p -> IR.PrimTy p
    IR.Prim p -> IR.Prim (CoreApp.Return (ty :| []) p)
    IR.Pi u x y -> IR.Pi u (baseToReturn ty x) (baseToReturn ty y)
    IR.Lam t -> IR.Lam (baseToReturn ty t)
    IR.Sig u x y -> IR.Sig u (baseToReturn ty x) (baseToReturn ty y)
    IR.Pair x y -> IR.Pair (baseToReturn ty x) (baseToReturn ty y)
    IR.Let u a b -> IR.Let u (elimToReturn ty a) (baseToReturn ty b)
    IR.UnitTy -> IR.UnitTy
    IR.Unit -> IR.Unit
    IR.Elim e -> IR.Elim (elimToReturn ty e)

elimToReturn ::
  ty ->
  Elim' IR.NoExt ty val ->
  Elim' IR.NoExt ty (CoreApp.Return' IR.NoExt (NonEmpty ty) val) -- ty' --(TypedPrim ty val)
elimToReturn ty e =
  case e of
    IR.Bound b -> IR.Bound b
    IR.Free n -> IR.Free n
    IR.App e t -> IR.App (elimToReturn ty e) (baseToReturn ty t)
    IR.Ann u a b c -> IR.Ann u (baseToReturn ty a) (baseToReturn ty b) c

unsafeEval ::
  IR.CanEval IR.NoExt IR.NoExt primTy primVal =>
  IR.RawGlobals primTy primVal ->
  IR.Term primTy primVal ->
  IR.Value primTy primVal
unsafeEval globals = fromRight . IR.evalTerm (IR.rawLookupFun' globals)
  where
    fromRight ~(Right x) = x
