module Frontend.Parser where

import qualified Criterion.Main as Criterion
import Data.Attoparsec.ByteString
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (mod)
import Prelude (String)

--------------------------------------------------------------------------------
-- Bench Groups
--------------------------------------------------------------------------------

bench :: Criterion.Benchmark
bench =
  Criterion.bgroup
    "parser"
    [let', mod]

let' :: Criterion.Benchmark
let' =
  Criterion.bgroup
    "let"
    [ Criterion.bench "let foo = 3 WHNF 17" $
        Criterion.whnf letParser str17,
      Criterion.bench "let foo = 3 NF 17" $
        Criterion.nf letParser str17,
      Criterion.bench "let foo = 3 WHNF 50" $
        Criterion.whnf letParser str50,
      Criterion.bench "let foo = 3 NF 50" $
        Criterion.nf letParser str50
    ]
  where
    str17 =
      "let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo \
      \= 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let \
      \foo = 3 let foo = 3 let foo = 3 let foo = 3 "
    str50 =
      "let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let \
      \ foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo \
      \ = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = \
      \ 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 \
      \ let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let \
      \ foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo \
      \ = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = \
      \ 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 let foo = 3 "

mod :: Criterion.Benchmark
mod =
  Criterion.bgroup
    "module"
    [ Criterion.bench "module Validation" $
        Criterion.nf topLevelParser example1
    ]
  where
    example1 =
      "mod Validation = \n"
        <> "  let T = Token.T -> Transaction.T -> Bool \n"
        <> " \n"
        <> "  let mint token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Mint -> \n"
        <> "      token.owner == tx-tx-authorized-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> " \n"
        <> "  let transfer token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Transfer {from-account, amount} -> \n"
        <> "      has-n token.storage.accounts from-account amount \n"
        <> "      && tx.authroized-account == from-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> " \n"
        <> "  let Burn token tx = \n"
        <> "    case tx.data of \n"
        <> "    | Transaction.Burn {burn-from-account, burn-ammount} -> \n"
        <> "      has-n token.storage.accounts burn-from-account burn-amount \n"
        <> "      && tx.authroized-account == burn-from-account \n"
        <> "    | _ -> \n"
        <> "      false \n"
        <> "end \n"

--------------------------------------------------------------------------------
-- Parser functions
--------------------------------------------------------------------------------

letParser :: ByteString -> Either String [Types.Let]
letParser = parseOnly (many' (Parser.spaceLiner Parser.let'))

topLevelParser :: ByteString -> Either String Types.TopLevel
topLevelParser = parseOnly Parser.topLevel
