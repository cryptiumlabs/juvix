module Main

import Tezos

%default total

-- A tiny hack for now.
run__IO : a -> a
run__IO f = f

-- Main contract function.
main : (String, String) -> (List Operation, String)
main _ = (Nil, "")

-- TODO: Shareholder DAO with ranked-choice voting over mutually exclusive proposals.
