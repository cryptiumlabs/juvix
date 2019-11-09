module Juvix.Backends.Michelson.Compilation.Util where

import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List                                  (elemIndex)
import qualified Data.Text                                  as T
import           Protolude

import           Juvix.Backends.Michelson.Compilation.Types
import qualified Juvix.Backends.Michelson.Untyped           as M
import           Juvix.Lang
import           Juvix.Utility

pack ∷ forall m . MonadError CompilationError m ⇒ M.Type → m M.Expr
pack M.UnitT = return (M.Const M.Unit)
pack ty      = throw (NotYetImplemented ("pack: " `T.append` prettyPrintValue ty))

-- Start with value to unpack at top of stack. len (filter Just binds) will be dropped at the end.
unpack ∷ forall m . (MonadError CompilationError m, MonadState M.Stack m) ⇒ M.Type → [Maybe T.Text] → m M.Expr
unpack M.BoolT [] = do
  modify (drop 1)
  return M.Nop
unpack ty@(M.PairT fT sT) binds =
  case binds of
    [Just fst, Just snd] → do
      modify ((<>) [(M.VarE fst, fT), (M.VarE snd, sT)] . drop 1)
      return (M.Seq (M.Seq (M.Seq M.Dup M.Cdr) M.Swap) M.Car)
    [Just fst, Nothing] → do
      modify ((:) (M.VarE fst, fT) . drop 1)
      return M.Car
    [Nothing, Just snd] → do
      modify ((:) (M.VarE snd, sT) . drop 1)
      return M.Cdr
    [Nothing, Nothing]  → do
      genReturn M.Drop
    _ → throw (NotYetImplemented (T.concat ["unpack: ", prettyPrintValue ty, " ~ ", T.intercalate ", " (fmap prettyPrintValue binds)]))
unpack ty binds = throw (NotYetImplemented (T.concat ["unpack: ", prettyPrintValue ty, " ~ ", T.intercalate ", " (fmap prettyPrintValue binds)]))

unpackDrop ∷ forall m . (MonadError CompilationError m, MonadState M.Stack m) ⇒ [Maybe T.Text] → m M.Expr
unpackDrop binds = genReturn (foldDrop (length (filter isJust binds)))

position ∷ T.Text → M.Stack → Maybe Int
position n []     = Nothing
position n (x:xs) = if fst x == M.VarE n then Just 0 else (+) 1 |<< position n xs

dropFirst ∷ T.Text → M.Stack → M.Stack
dropFirst n (x:xs) = if fst x == M.VarE n then xs else x : dropFirst n xs
dropFirst _ []     = []

unitaryTypes ∷ [M.Type]
unitaryTypes = [M.UnitT, M.IntT, M.TezT, M.KeyT, M.BoolT]

genReturn ∷ forall m . (MonadError CompilationError m, MonadState M.Stack m) ⇒ M.Expr → m M.Expr
genReturn expr = do
  modify =<< genFunc expr
  return expr

genFunc ∷ forall m . MonadError CompilationError m ⇒ M.Expr → m (M.Stack → M.Stack)
genFunc expr =

  case expr of
    M.Nop -> return (\x -> x)

    M.Drop -> return (drop 1)
    M.Dup -> return (\(x:xs) -> x:x:xs)
    M.Swap -> return (\(x:y:xs) -> y:x:xs)

    M.Amount -> return ((:) (M.FuncResult, M.TezT))

    M.Nil t -> return ((:) (M.FuncResult, M.ListT t))

    M.Car -> return (\( (_, M.PairT x _) : xs) -> (M.FuncResult, x) : xs)
    M.Cdr -> return (\( (_, M.PairT _ y) : xs) -> (M.FuncResult, y) : xs)

    M.ConsPair -> return (\( (_, xT) : (_, yT) : xs) -> (M.FuncResult, M.PairT xT yT) : xs)

    M.AddIntInt -> return ((:) (M.FuncResult, M.IntT) . drop 2)
    M.MulIntInt -> return ((:) (M.FuncResult, M.IntT) . drop 2)

    M.Dip x -> do
      f <- genFunc x
      return (\(x:xs) -> x : f xs)

    M.Seq x y → do
      x ← genFunc x
      y ← genFunc y
      return (y . x)

    M.Const (M.Bool _) → return ((:) (M.FuncResult, M.BoolT))

    _ → throw (NotYetImplemented (T.concat ["genFunc: ", prettyPrintValue expr]))

isNilList ∷ M.Expr → Bool
isNilList (M.Nil _) = True
isNilList _         = False
