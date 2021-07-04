module Juvix.Sexp.Structure.EffectHandlerHelpers where

import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Sexp.Structure.GenStructures as Str
import qualified Juvix.Library.NameSymbol as NameSymbol

data Via = Via
  {
    viaHandler :: Str.LetHandler,
    viaProgram :: Str.Do
  }

data Handler = Handler
 {
   handlerName :: NameSymbol.T,
   handlerRet  :: Str.LetRet,
   handlerOps  :: [Str.LetOp]
 }

nameVia :: NameSymbol.T
nameVia = ":via"

isVia :: Sexp.T -> Bool
isVia (Sexp.Cons form _) = Sexp.isAtomNamed form nameVia
isVia _ = False

toVia :: Sexp.T -> Maybe Via
toVia form
  | isVia form =
    case form of
      _ Sexp.:> hand Sexp.:> prog Sexp.:> Sexp.Nil ->
        Via <$> toHandler hand <*> Str.toDo prog
      _ -> Nothing
  | otherwise = Nothing

toHandler :: Sexp.T -> Maybe Str.LetHandler
toHandler form =
  let hand = Str.toLetHandler form
      ret  = fmap (Str.toLetRet . Str.letHandlerRet) hand
      ops  = fmap (toLetOps . Str.letHandlerOps)  hand
  in Str.LetHandler <$> Str.letHandlerName hand <*> ret <*> ops

toLetOps :: Sexp.T -> Maybe [Str.LetOp]
toLetOps form =
  let ops = Sexp.toList form
  in undefined
