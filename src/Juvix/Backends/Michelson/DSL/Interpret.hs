{-# LANGUAGE ViewPatterns #-}

module Juvix.Backends.Michelson.DSL.Interpret where

import qualified Data.Map as Map
import qualified Data.Singletons as Single
import qualified Data.Vinyl.Core as Vinyl
import qualified Fmt as Fmt
import qualified Juvix.Backends.Michelson.Compilation.Types as Types
import qualified Juvix.Backends.Michelson.DSL.Contract as Contract
import Juvix.Library
import qualified Michelson.Interpret as Interpret
import qualified Michelson.Text as Text
import qualified Michelson.Typed.Aliases as TAlias
import qualified Michelson.Typed.Convert as Convert
import qualified Michelson.Typed.EntryPoints as Entry
import qualified Michelson.Typed.Instr as Instr
import qualified Michelson.Typed.Scope as Scope
import qualified Michelson.Typed.Value as TValue
import qualified Michelson.Untyped.Aliases as Alias
import qualified Michelson.Untyped.Value as Value
import qualified Tezos.Core as Core
import qualified Tezos.Crypto as Crypto

dummyInterpretContract ::
  Alias.Contract -> Either Interpret.InterpretError Interpret.InterpretResult
dummyInterpretContract contract =
  Interpret.interpretUntyped
    contract
    Value.ValueUnit
    Value.ValueUnit
    Contract.dummyContractEnv

dummyInterpret ::
  Types.EmptyInstr2 ->
  Either Interpret.MichelsonFailed Alias.Value
dummyInterpret (Types.EmptyInstr2 inst) =
  case Interpret.interpretInstr Contract.dummyContractEnv inst Vinyl.RNil of
    Left _ -> undefined
    Right ((x :: TAlias.Value t) Vinyl.:& _) ->
      case Scope.checkOpPresence (Single.sing @t) of
        Scope.OpPresent -> undefined
        Scope.OpAbsent -> pure (Convert.untypeValue x)
    Right _ -> undefined

untypeValue :: TValue.Value' Instr.Instr t -> Alias.Value
untypeValue val =
  case val of
    TValue.VInt i ->
      Value.ValueInt i
    TValue.VNat i ->
      Value.ValueInt (toInteger i)
    TValue.VString s ->
      Value.ValueString s
    TValue.VBytes b ->
      Value.ValueBytes (Value.InternalByteString b)
    TValue.VMutez m ->
      Value.ValueInt (toInteger (Core.unMutez m))
    TValue.VBool True ->
      Value.ValueTrue
    TValue.VBool False ->
      Value.ValueFalse
    TValue.VKeyHash h ->
      Value.ValueString (Crypto.mformatKeyHash h)
    TValue.VTimestamp t ->
      Value.ValueString (Text.mkMTextUnsafe (Fmt.pretty t))
    TValue.VAddress a ->
      Value.ValueString (Entry.mformatEpAddress a)
    TValue.VKey b ->
      Value.ValueString (Crypto.mformatPublicKey b)
    TValue.VUnit ->
      Value.ValueUnit
    TValue.VSignature b ->
      Value.ValueString (Crypto.mformatSignature b)
    TValue.VChainId b ->
      Value.ValueString (Core.mformatChainId b)
    TValue.VOption (Just s) ->
      Value.ValueSome (untypeValue s)
    TValue.VOption Nothing ->
      Value.ValueNone
    TValue.VList l ->
      vList Value.ValueSeq (untypeValue <$> l)
    TValue.VSet s ->
      vList Value.ValueSeq (untypeValue <$> toList s)
    TValue.VContract addr sepc ->
      Value.ValueString (Entry.mformatEpAddress (Entry.EpAddress addr (Entry.sepcName sepc)))
    TValue.VPair (l, r) ->
      Value.ValuePair (untypeValue l) (untypeValue r)
    TValue.VOr (Left x) ->
      Value.ValueLeft (untypeValue x)
    TValue.VOr (Right x) ->
      Value.ValueRight (untypeValue x)
    TValue.VLam (TValue.rfAnyInstr -> ops :: Instr.Instr '[inp] '[out]) ->
      vList Value.ValueLambda $ Convert.instrToOps ops
    TValue.VMap m ->
      vList Value.ValueMap $
        Map.toList m
          <&> \(k, v) ->
            Value.Elt (untypeValue k) (untypeValue v)
    TValue.VBigMap m ->
      vList Value.ValueMap $
        Map.toList m
          <&> \(k, v) ->
            Value.Elt (untypeValue k) (untypeValue v)
  where
    vList ctor = maybe Value.ValueNil ctor . nonEmpty
