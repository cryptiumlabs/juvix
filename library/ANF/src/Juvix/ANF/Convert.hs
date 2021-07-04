module Juvix.ANF.Convert where

import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Library.Sexp.Structure as Str
import Juvix.ANF.IR

convert :: Has2Closures m n => HandlerContext m n  -> HandlerContext m n
convert sexp = Sexp.foldPred sexp isEffectful conv
  where
    convVia = convertVia . mapViaStr . toVia
    conv atom cdr
      | Str.isVia    = fmap (Sexp.Cons cdr) (conVia atom)
      | otherwise    = error "something weird happened"

-- convHandler :: Has2Closures m n => HandlerContext m n  -> HandlerContext m n
convHandler prog = do
  pure $ Sexp.list
    [ Sexp.atom Str.nameLetMatch,
      undefined,
      matchCases prog,
      forward prog
    ]
  where
    matchCases prog
      | Str.isOp  = undefined
      | Str.isRet = undefined

    forward prog = do
      h <- get @"pure"
      k <- get @"effectful"
      h' <- get @"pure"
      k' <- get @"effectful"
      pure $ undefined

-- convertDo :: Has2Closures m n => HandlerContext m n -> HandlerContext m n
convertDo = do
   h <- get @"pure"
   k <- get @"effectful"
   pure $ -- undefined l (V, \x -> k x h)


convertVia { }



-- vmap relays continuations to outer handlers if necessary
-- vmap :: Has2Closures m n => HandlerContext m n -> HandlerContext m n
vmap f val prog = do
  h <- get @"pure"
  return $ f val (\x -> k x) prog
