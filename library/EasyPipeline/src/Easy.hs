-- |
-- The easy module serves as the stop shop for getting anywhere in the
-- code-base fast.
--
-- _The file is laid out where_
--  1. we lay out a phase
--     - We have 2 variants of each phase
--       1) <name>File
--       2) <name>Library
--     - This lasts up until context, as we can see if the prelude we
--       give it matches our expectations
--  2. We then give examples
--
-- We do 1. and 2. having each step rely on the last, and continue the
-- process until the compiler is at the full backends.
--
-- We can view this approach as giving us a quick way to play around
-- with any stage of the compiler while modifying the source code.
module Easy where

import qualified Juvix.Backends.Michelson.Parameterisation as Param
import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as ResolveOpen
import qualified Juvix.Contextify.ToContext.Types as ContextifyT
import qualified Juvix.Core as Core
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Core.Common.Context.Traverse as Traverse
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend as Frontend
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types as FrontendT
import qualified Juvix.Frontend.Types as Initial
import qualified Juvix.Frontend.Types.Base as Frontend
import qualified Juvix.FrontendDesugar as FrontDesugar
import Juvix.Library
import qualified Juvix.Library.Feedback as Feedback
import qualified Juvix.Library.NameSymbol as NameSymb
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Pipeline as Pipeline
import qualified Juvix.Pipeline.Compile as Compile
import Text.Pretty.Simple (pPrint)
import Prelude (error)

--------------------------------------------------------------------------------
-- OPTIONS
--------------------------------------------------------------------------------

data Options = Opt
  { prelude :: [FilePath],
    currentContextName :: NameSymb.T
  }
  deriving (Show)

-- we can override defaults by saying def { newOptions }
def :: Options
def =
  Opt
    { -- to avoid being overhwlemed in the repl by giant text, we have
      -- a minimal file here. Our functions will take def, so we can
      -- replace it by the full library
      prelude = ["juvix/minimal.ju"],
      -- by default our code will live in Juvix-User
      currentContextName = "Juvix-User"
    }

-- @defFull@ gives us the entire prelude
defFull :: Options
defFull =
  def
    { prelude =
        [ "../../stdlib/Prelude.ju",
          "../../stdlib/Michelson.ju",
          "../../stdlib/MichelsonAlias.ju"
        ]
    }

-- These functions help us stop at various part of the pipeline

--------------------------------------------------------------------------------
-- SEXP PHASE
--------------------------------------------------------------------------------

-- | here we stop at the first stage of the first step of the compiler
-- Text ⟶ ML AST ⟶ LISP AST
-- You may want to stop here if you want to see what some base forms look like
sexp :: ByteString -> [Sexp.T]
sexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

-- | Here we extend the idea of desugar but we run it on the prelude we
-- care about.
-- File ⟶ ML AST ⟶ LISP AST
sexpFile :: FilePath -> IO [Sexp.T]
sexpFile file = do
  f <- Frontend.ofSingleFile file
  case f of
    Right (_name, ast) ->
      fmap SexpTrans.transTopLevel ast
        |> pure
    Left err ->
      error (show err)

-- | here we run the sexp transformation on the library
-- Prelude ⟶ ML AST ⟶ LISP AST
sexpLibrary :: Options -> IO [(NameSymb.T, [Sexp.T])]
sexpLibrary def = do
  files <- Frontend.ofPath (prelude def)
  case files of
    Right f ->
      pure (second (fmap SexpTrans.transTopLevel) <$> f)
    Left err ->
      error (show err)

----------------------------------------
-- SEXP Examples
----------------------------------------

-- here are some sexp examples you may want to play with

sexp1, sexp2 :: [Sexp.T]
sexp1 = sexp "type list a : ty -> ty = Cons a (List a) | Nil"
sexp2 =
  sexp
    "let foo (Cons x xs) = x + foo xs\
    \ let foo Nil = 0"

--------------------------------------------------------------------------------
-- DESUGAR PHASE
--------------------------------------------------------------------------------

-- | Here is our second stop of the compiler, we now run the desugar passes
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP
-- you may want to stop here if you want to see the syntax before we
-- get dirtier output from everything being in the contex
desugar :: ByteString -> [Sexp.T]
desugar = Desugar.op . sexp

-- | This is like Desugar but our pipeline looks like
-- LISP AST ⟶ De-sugared LISP
desugarLisp :: [Sexp.T] -> [Sexp.T]
desugarLisp = Desugar.op

-- | Here we extend the idea of desugar but we run it on the file we
-- care about.
-- File ⟶ … ⟶ De-sugared LISP
desugarFile :: FilePath -> IO [Sexp.T]
desugarFile = fmap desugarLisp . sexpFile

-- | @desugarLibrary@ is run on the library to get the s-expression
-- Prelude ⟶ … ⟶ De-sugared LISP
desugarLibrary :: Options -> IO [(NameSymb.T, [Sexp.T])]
desugarLibrary def = do
  lib <- sexpLibrary def
  pure (second desugarLisp <$> lib)

----------------------------------------
-- DESUGAR Examples
----------------------------------------

desugar1, desugar2 :: [Sexp.T]
desugar1 = desugarLisp sexp2
desugar2 =
  desugar
    "let fi = \
    \ let foo (Cons x xs) True = foo xs False in \
    \ let foo (Nil) t = t in \
    \ foo [1,2,3,4]"

-- Example of the minimal prelude if you want to investigate it
desugarMinimalPrelude :: IO [(NameSymb.T, [Sexp.T])]
desugarMinimalPrelude = desugarLibrary def

--------------------------------------------------------------------------------
-- Context Phase
--------------------------------------------------------------------------------

-- | Here is our third stop in the compiler, we are now upon the context.
-- we need to also load up the AST
-- Text ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP
-- You may want to stop here if you want to see the context before
-- resolving the opens
contextifyNoResolve ::
  ByteString ->
  Options ->
  IO (Contextify.PathError (ContextifyT.ContextSexp, [ResolveOpen.PreQualified]))
contextifyNoResolve text def = do
  lib <- desugarLibrary def
  let dusugared = desugar text
  Contextify.contextify (((currentContextName def), dusugared) :| lib)

-- | We do @contextifyNoResolve@ but on a file instead
-- File ⟶ ML AST ⟶ LISP AST ⟶ De-sugared LISP ⟶ Contextified LISP
contextifyNoResolveFile ::
  FilePath ->
  Options ->
  IO (Contextify.PathError (ContextifyT.ContextSexp, [ResolveOpen.PreQualified]))
contextifyNoResolveFile file def = do
  lib <- desugarLibrary def
  dusugared <- desugarFile file
  Contextify.contextify (((currentContextName def), dusugared) :| lib)

contextitfyNoResolve1 ::
  IO (Contextify.PathError (ContextifyT.ContextSexp, [ResolveOpen.PreQualified]))
contextitfyNoResolve1 =
  contextifyNoResolve
    "let fi = \
    \ let foo (Cons x xs) True = foo xs False in \
    \ let foo (Nil) t = t in \
    \ foo [1,2,3,4]"
    def

-- At this point the context is a bit unreadable, so to make our lives
-- easier we can write instead

contextifyNoResolve1Pretty :: IO ()
contextifyNoResolve1Pretty = do
  Right (ctx, _resolve) <- contextitfyNoResolve1
  printDefDefModule def ctx

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | @printModule@ prints the module given to it
printModule ::
  (MonadIO m, Show ty, Show term, Show sumRep) => NameSymb.T -> Context.T term ty sumRep -> m ()
printModule name ctx =
  case Context.inNameSpace name ctx of
    Just ctx ->
      pPrint (Context.currentNameSpace ctx)
    Nothing ->
      pure ()

printDefDefModule ::
  (MonadIO m, Show ty, Show term, Show sumRep) => Options -> Context.T term ty sumRep -> m ()
printDefDefModule def = printModule (currentContextName def)

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"
