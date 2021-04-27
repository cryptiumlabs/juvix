module Encoding where

import Juvix.Encoding.Encoding
import Juvix.Encoding.Mendler
import Juvix.Encoding.Scott
import Juvix.Encoding.Types
import Juvix.Library hiding (Product, Sum)

userNat :: Name
userNat =
  Adt "Nat" (Branch "Z" None (Single "S" Term))

dUserNat :: Name
dUserNat =
  Adt "Nat" (Branch "Z" None (Branch "S" Term (Single "D" (Product Term))))

-- Test cases for Nat ----------------------------------------------------------
zero' :: Lambda
zero' = app in' (app inl (Lambda "x" (Value "x")))

succ' :: Lambda
succ' =
  Lambda "c%gen1" (app in' (app inr (app inl (Value "c%gen1"))))

dup' :: Lambda
dup' =
  Lambda
    "c%gen1"
    ( Lambda
        "c%gen2"
        ( app
            in'
            ( app
                inr
                ( app
                    inrOp
                    ( Lambda
                        "%fun"
                        ( Application
                            ( Application
                                (Value "%fun")
                                (Value "c%gen1")
                            )
                            (Value "c%gen2")
                        )
                    )
                )
            )
        )
    )

test2D :: Either Errors (Lambda, Env)
test2D = runEnvsS $ do
  adtToMendler dUserNat
  mendlerCase
    ( Case
        (Value "val")
        [ C
            "Z"
            []
            (Value "True"),
          C
            "S"
            ["n"]
            ( Application
                (Value "not")
                ( Application
                    (Value "rec")
                    (Value "n")
                )
            ),
          C
            "D"
            [ "n1",
              "n2"
            ]
            ( Application
                (Value "not")
                ( Application
                    (Value "rec")
                    (Value "n1")
                )
            )
        ]
    )

-- let rec f x i =
--   case x of
--   | Z       → i
--   | S n     → 1 + (f n i)
--   | D n1 n2 → f n2 0 + f n1 i

test3D :: Either Errors (Lambda, Env)
test3D = runEnvsS $ do
  adtToMendler dUserNat
  mendlerCase
    ( Case
        (Value "val")
        [ C
            "Z"
            []
            (Value "i"),
          C
            "S"
            ["n"]
            ( Application
                ( Application
                    (Value "+")
                    (Value "1")
                )
                ( Application
                    ( Application
                        (Value "rec")
                        (Value "n")
                    )
                    (Value "i")
                )
            ),
          C
            "D"
            [ "n1",
              "n2"
            ]
            ( Application
                ( Application
                    (Value "+")
                    ( Application
                        ( Application
                            (Value "rec")
                            (Value "n2")
                        )
                        (Value "0")
                    )
                )
                ( Application
                    ( Application
                        (Value "rec")
                        (Value "n1")
                    )
                    (Value "i")
                )
            )
        ]
    )

test3D' :: Either Errors (Lambda, Env)
test3D' = runEnvsS $ do
  adtToScott dUserNat
  scottCase
    ( Case
        (Value "val")
        [ C
            "Z"
            []
            (Value "i"),
          C
            "S"
            ["n"]
            ( Application
                ( Application
                    (Value "+")
                    (Value "1")
                )
                ( Application
                    ( Application
                        (Value "rec")
                        (Value "n")
                    )
                    (Value "i")
                )
            ),
          C
            "D"
            [ "n1",
              "n2"
            ]
            ( Application
                ( Application
                    (Value "+")
                    ( Application
                        ( Application
                            (Value "rec")
                            (Value "n2")
                        )
                        (Value "0")
                    )
                )
                ( Application
                    ( Application
                        (Value "rec")
                        (Value "n1")
                    )
                    (Value "i")
                )
            )
        ]
    )

test1 :: Either Errors (Lambda, Env)
test1 = runEnvsS $ do
  adtToMendler userNat
  mendlerCase
    ( Case
        (Value "val")
        [ C
            "Z"
            []
            (Value "True"),
          C
            "S"
            ["n"]
            ( Application
                (Value "not")
                ( Application
                    (Value "rec")
                    (Value "n")
                )
            )
        ]
    )

test1' :: Either Errors (Lambda, Env)
test1' = runEnvsS $ do
  adtToScott userNat
  scottCase
    ( Case
        (Value "val")
        [ C
            "Z"
            []
            (Value "True"),
          C
            "S"
            ["n"]
            ( Application
                (Value "not")
                ( Application
                    (Value "rec")
                    (Value "n")
                )
            )
        ]
    )
