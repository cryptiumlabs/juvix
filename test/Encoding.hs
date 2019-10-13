module Encoding where

import Juvix.Encoding.Encoding
import Juvix.Encoding.Mendler
import Juvix.Encoding.Scott
import Juvix.Encoding.Types
import Juvix.Library hiding (Product, Sum)

userNat ∷ Name

userNat =
  Adt
    (intern "Nat")
    ( Branch
        (intern "Z")
        None
        (Single (intern "S") Term)
    )

dUserNat ∷ Name

dUserNat =
  Adt
    (intern "Nat")
    ( Branch
        (intern "Z")
        None
        ( Branch
            (intern "S")
            Term
            (Single (intern "D") (Product Term))
        )
    )

-- Test cases for Nat ----------------------------------------------------------
zero' ∷ Lambda

zero' =
  app
    in'
    ( app
        inl
        ( Lambda
            (intern "x")
            (Value (intern "x"))
        )
    )

succ' ∷ Lambda

succ' =
  Lambda
    (intern "c%gen1")
    (app in' (app inr (app inl (Value (intern "c%gen1")))))

dup' ∷ Lambda

dup' =
  Lambda
    (intern "c%gen1")
    ( Lambda
        (intern "c%gen2")
        ( app
            in'
            ( app
                inr
                ( app
                    inrOp
                    ( Lambda
                        (intern "%fun")
                        ( Application
                            ( Application
                                (Value $ intern "%fun")
                                (Value $ intern "c%gen1")
                            )
                            (Value $ intern "c%gen2")
                        )
                    )
                )
            )
        )
    )

test2D ∷ Either Errors (Lambda, Env)

test2D = runEnvsS $ do
  adtToMendler dUserNat
  mendlerCase
    ( Case
        (Value $ intern "val")
        [ C
            (intern "Z")
            []
            (Value $ intern "True"),
          C
            (intern "S")
            [intern "n"]
            ( Application
                (Value $ intern "not")
                ( Application
                    (Value $ intern "rec")
                    (Value $ intern "n")
                )
            ),
          C
            (intern "D")
            [ intern "n1",
              intern "n2"
            ]
            ( Application
                (Value $ intern "not")
                ( Application
                    (Value $ intern "rec")
                    (Value $ intern "n1")
                )
            )
        ]
    )

-- let rec f x i =
--   case x of
--   | Z       -> i
--   | S n     -> 1 + (f n i)
--   | D n1 n2 -> f n2 0 + f n1 i

test3D ∷ Either Errors (Lambda, Env)

test3D = runEnvsS $ do
  adtToMendler dUserNat
  mendlerCase
    ( Case
        (Value $ intern "val")
        [ C
            (intern "Z")
            []
            (Value $ intern "i"),
          C
            (intern "S")
            [intern "n"]
            ( Application
                ( Application
                    (Value $ intern "+")
                    (Value $ intern "1")
                )
                ( Application
                    ( Application
                        (Value $ intern "rec")
                        (Value $ intern "n")
                    )
                    (Value $ intern "i")
                )
            ),
          C
            (intern "D")
            [ intern "n1",
              intern "n2"
            ]
            ( Application
                ( Application
                    (Value $ intern "+")
                    ( Application
                        ( Application
                            (Value $ intern "rec")
                            (Value $ intern "n2")
                        )
                        (Value $ intern "0")
                    )
                )
                ( Application
                    ( Application
                        (Value $ intern "rec")
                        (Value $ intern "n1")
                    )
                    (Value $ intern "i")
                )
            )
        ]
    )

test3D' ∷ Either Errors (Lambda, Env)

test3D' = runEnvsS $ do
  adtToScott dUserNat
  scottCase
    ( Case
        (Value $ intern "val")
        [ C
            (intern "Z")
            []
            (Value $ intern "i"),
          C
            (intern "S")
            [intern "n"]
            ( Application
                ( Application
                    (Value $ intern "+")
                    (Value $ intern "1")
                )
                ( Application
                    ( Application
                        (Value $ intern "rec")
                        (Value $ intern "n")
                    )
                    (Value $ intern "i")
                )
            ),
          C
            (intern "D")
            [ intern "n1",
              intern "n2"
            ]
            ( Application
                ( Application
                    (Value $ intern "+")
                    ( Application
                        ( Application
                            (Value $ intern "rec")
                            (Value $ intern "n2")
                        )
                        (Value $ intern "0")
                    )
                )
                ( Application
                    ( Application
                        (Value $ intern "rec")
                        (Value $ intern "n1")
                    )
                    (Value $ intern "i")
                )
            )
        ]
    )

test1 ∷ Either Errors (Lambda, Env)

test1 = runEnvsS $ do
  adtToMendler userNat
  mendlerCase
    ( Case
        (Value $ intern "val")
        [ C
            (intern "Z")
            []
            (Value $ intern "True"),
          C
            (intern "S")
            [intern "n"]
            ( Application
                (Value $ intern "not")
                ( Application
                    (Value $ intern "rec")
                    (Value $ intern "n")
                )
            )
        ]
    )

test1' ∷ Either Errors (Lambda, Env)

test1' = runEnvsS $ do
  adtToScott userNat
  scottCase
    ( Case
        (Value $ intern "val")
        [ C
            (intern "Z")
            []
            (Value $ intern "True"),
          C
            (intern "S")
            [intern "n"]
            ( Application
                (Value $ intern "not")
                ( Application
                    (Value $ intern "rec")
                    (Value $ intern "n")
                )
            )
        ]
    )
