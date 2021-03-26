{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Juvix.Pipeline.Compile (
  Pipeline,
  parse,
  toCoreDef,
  isMain,
  convGlobal,
  unsafeEvalGlobal,
  writeout
) where

import qualified Data.Text as Text
import qualified Data.Text.IO as T
import qualified Juvix.Core.Application as CoreApp
import qualified Juvix.Core.IR as IR
import Juvix.Core.IR.Types.Base
import Juvix.Core.IR.Types.Globals
import Juvix.ToCore.Types (CoreDef(..))
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Pipeline.Internal as Pipeline
import qualified System.IO.Temp as Temp
import qualified Prelude as P
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Core.Common.Context as Context

type Code = Text

type OutputCode = Text

type Message = P.String

type Pipeline = Feedback.FeedbackT [] Message IO

parse :: Code -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)
parse code = do
  core <- liftIO $ toCore_wrap code
  case core of
    Right ctx -> return ctx
    Left err -> Feedback.fail $ show err
  where
    toCore_wrap :: Code -> IO (Either Pipeline.Error (Context.T Sexp.T Sexp.T Sexp.T))
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
unsafeEval globals = either (panic "Failed to eval term") identity . IR.evalTerm (IR.rawLookupFun' globals)