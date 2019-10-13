module Juvix.CodeGen where

import Control.Monad.Except
import Control.Monad.Writer
import IRTS.CodegenCommon
import IRTS.Compiler
import IRTS.Lang
import IRTS.Simplified
import Idris.AbsSyntax
import Idris.Core.Evaluate
import Idris.Core.TT hiding (Name, Type)
import Idris.ElabDecls
import Idris.Main
import Idris.Options
import qualified Juvix.Backends.Michelson as M
import Juvix.Lang
import Juvix.Utility
import Protolude hiding (Type)

data Opts
  = Opts
      { inputs :: [FilePath],
        interface :: Bool,
        output :: FilePath
      }

showUsage :: IO ()

showUsage = do
  putText "A code generator which is intended to be called by the compiler, not by a user."
  putText "Usage: idris-codegen-juvix <ibc-files> [-o <output-file>]"
  exitWith ExitSuccess

getOpts :: IO Opts

getOpts = do
  xs <- getArgs
  return $ process (Opts [] False "main.jvx") xs
  where
    process opts ("-o" : o : xs) = process (opts {output = o}) xs
    process opts ("--interface" : xs) = process (opts {interface = True}) xs
    process opts (x : xs) = process (opts {inputs = x : inputs opts}) xs
    process opts [] = opts

codeGenSdecls :: Type → CodeGenerator

codeGenSdecls ty ci = do
  putText $ "Type: " <> show ty
  let decls = liftDecls ci
  let main = findMain decls
  let decl = snd main
  let LFun _ _ args body = decl
      expr = LLam args body
  putText $ "Main expr prettified: " <> prettyPrintValue expr
  case runWriter $ runExceptT (M.compileToMichelsonSourceFile expr ty :: (ExceptT M.CompilationError (Writer [M.CompilationLog]) Text)) of
    (res, logs) -> do
      mapM_ (putText . prettyPrintValue) logs
      case res of
        Right output -> do
          writeFile (outputFile ci) output
        Left err -> do
          putText $ "Error during transpilation: " <> prettyPrintValue err
          exitFailure

mainN :: Name

mainN = NS (UN "main") ["Main"]

findMain :: [(Name, LDecl)] → (Name, LDecl)

findMain decls = let Just f = head $ filter (\(name, _) -> name == mainN) decls in f

sdeclMain :: Opts → Idris ()

sdeclMain opts = do
  elabPrims
  _ <- loadInputs (inputs opts) Nothing
  mainProg <- if interface opts then liftM Just elabMain else return Nothing
  ir <- compile (Via IBCFormat "juvix") (output opts) mainProg
  is <- getIState
  let Just ty = lookupTyExact mainN (tt_ctxt is)
  runIO $ codeGenSdecls ty ir

main :: IO ()

main = do
  opts <- getOpts
  if (null (inputs opts))
    then showUsage
    else runMain (sdeclMain opts)
