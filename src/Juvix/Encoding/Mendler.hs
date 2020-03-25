{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- |
-- - Gives a =Mendler= encoding from an =ADT=.
-- - This file is not done as Ι have yet to find a way to convert any
--   general recursive function into a proper =Mendler= form.
-- - This however will do the boilerplate of changing an =ADT= to
--   =Mendler= and the structure of a case expression into proper
--   =Mendler= form
module Juvix.Encoding.Mendler where

import Juvix.Encoding.Encoding
import Juvix.Encoding.Types
import Juvix.Library hiding (Product, Sum)
import qualified Juvix.Library.HashMap as Map
import Prelude (error)

-- TODO ∷ Properly setup a function transfrom to make it self terminate

-- | adtToMendler converts an adt into an environment where the mendler
-- encoding is defined for case functions
adtToMendler ::
  ( HasState "constructors" (Map.T Symbol Bound) m,
    HasState "adtMap" (Map.T Symbol Branches) m,
    HasThrow "err" Errors m
  ) =>
  Name ->
  m ()
adtToMendler (Adt name s) = sumRec s 0
  where
    sumRec (Single s p) posAdt = adtConstructor s (sumProd p posAdt) name
    sumRec (Branch s p nextSum) 0 =
      adtConstructor s (sumProd p 0) name
        *> sumRec nextSum 1
    -- So due to how the algorithm works for inr inl placement
    -- we need to incrase posAdt by 1 to reflect that it is a branch
    -- Note 0 is a special case as it is only inl never an inr
    sumRec (Branch s p nextSum) posAdt =
      adtConstructor s (sumProd p posAdt) name
        *> sumRec nextSum (posAdt + 2)
    sumProd None posAdt =
      numToIn
        posAdt
        ( Lambda
            (intern "x")
            (Value $ intern "x")
        )
    sumProd Term posAdt =
      Lambda
        (intern "%gen1")
        (numToIn posAdt (Value (intern "%gen1")))
    sumProd p@(Product _) posAdt = lambdas (numToInOp posAdt term)
      where
        (lambdas, term) = rec' p 0 (identity, (Value (intern "%fun")))
        rec' x index (lambdasBeforeIn, termToBuild) =
          let genI = intern ("%gen" <> show index)
              app = Application termToBuild (Value genI)
           in case x of
                Term ->
                  ( lambdasBeforeIn . Lambda genI,
                    Lambda (intern "%fun") app
                  )
                Product t ->
                  rec'
                    t
                    (succ index)
                    (lambdasBeforeIn . Lambda genI, app)
                None ->
                  ( lambdasBeforeIn,
                    Lambda
                      (intern "%fun")
                      ( Application
                          termToBuild
                          ( Lambda
                              (intern "x")
                              (Value $ intern "x")
                          )
                      )
                  )

mendlerCase ::
  ( HasState "constructors" (Map.T Symbol Bound) m,
    HasState "adtMap" (Map.T Symbol Branches) m,
    HasThrow "err" Errors m,
    HasWriter "missingCases" [Symbol] m
  ) =>
  Switch ->
  m Lambda
mendlerCase c = do
  expandedCase <- caseGen c onNoArg onrec
  case expandedCase of
    Application on b ->
      pure $ Application on $
        Lambda (intern "rec") b
    Lambda {} -> error "doesn't happen"
    Value {} -> error "doesn't happen"
  where
    onNoArg body = Lambda (intern "()") body
    onrec c accLam =
      Lambda
        (intern "c%gen")
        ( Application
            (Application (Value (intern "c%gen")) c)
            accLam
        )

-- Helpers for Mendler encoding ------------------------------------------------

numToInGen :: Int -> Lambda -> Lambda
numToInGen 0 arg = app in' arg
numToInGen n arg = app in' (rec' n arg)
  where
    rec' 0 acc = acc
    rec' 1 acc = acc
    rec' n acc = rec' (n - 2) (app inr acc)

-- Here is a chart that lays the relation between branch/single position and the number

-- t      ∧ Pos == 0 ⇒ inl ...             | 0
-- Signle ∧ Pos == 1 ⇒ inr ...             | 1
-- Branch ∧ Pos == 1 ⇒ inr (inl ...)       | 2
-- Single ∧ Pos == 2 ⇒ inr (inr ...)       | 3
-- Branch ∧ Pos == 2 ⇒ inr (inr (inl ...)) | 4

numToIn :: Int -> Lambda -> Lambda
numToIn n arg
  | even n = numToInGen n (app inl arg)
  | otherwise = numToInGen n (app inr arg)

numToInOp :: Int -> Lambda -> Lambda
numToInOp n arg
  | even n = numToInGen n (app inlOp arg)
  | otherwise = numToInGen n (app inrOp arg)

-- Lambda Abstraction for mendler encoding -------------------------------------

inl :: Lambda
inl =
  Lambda x
    $ Lambda k
    $ Lambda l
    $ Application (Value k) (Value x)
  where
    x = intern "x"
    k = intern "k"
    l = intern "l"

-- | Op of inl that has the first argument call the 2nd
-- useful for when constructing multiple argument passthrough
inlOp :: Lambda
inlOp =
  Lambda x
    $ Lambda k
    $ Lambda l
    $ Application (Value x) (Value k)
  where
    x = intern "x"
    k = intern "k"
    l = intern "l"

inr :: Lambda
inr =
  Lambda y
    $ Lambda k
    $ Lambda l
    $ Application (Value l) (Value y)
  where
    y = intern "y"
    k = intern "k"
    l = intern "l"

-- | Op of inr that has the first argument call the 2nd
-- useful for when constructing multiple argument passthrough
inrOp :: Lambda
inrOp =
  Lambda y
    $ Lambda k
    $ Lambda l
    $ Application (Value y) (Value l)
  where
    y = intern "y"
    k = intern "k"
    l = intern "l"

foldM' :: Lambda
foldM' = Lambda alg $ Lambda d $ Application (Value d) (Value alg)
  where
    alg = intern "alg"
    d = intern "d"

in' :: Lambda
in' =
  Lambda r
    $ Lambda f
    $ Application
      ( Application
          (Value f)
          (app foldM' (Value f))
      )
      (Value r)
  where
    r = intern "r"
    f = intern "f"
