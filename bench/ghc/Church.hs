{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Prelude (IO, print)

type Num = ∀ a. (a → a) → (a → a)

zero ∷ Num
zero s z = z

succ ∷ Num → Num
succ n s z = s (n s z)

two ∷ Num
two = succ (succ zero)

four ∷ Num
four = succ (succ (succ (succ zero)))

five ∷ Num
five = succ (succ (succ (succ (succ zero))))

ten ∷ Num
ten = succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero)))))))))

eleven ∷ Num
eleven = succ (succ (succ (succ (succ (succ (succ (succ (succ (succ (succ zero))))))))))

mul ∷ Num → Num → Num
mul a b s z = a (b s) z

fortyfour ∷ Num
fortyfour = mul four eleven

id ∷ a → a
id x = x

main ∷ IO ()
main = do
  print (fortyfour two id ())
