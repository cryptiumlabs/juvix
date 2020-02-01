module Token

open FStar
open FStar.All

module Map = FStar.OrdMap
module MapProp = FStar.OrdMapProps


(*******************************************************************)
(* Boiler plate taken from previous code *)
(*******************************************************************)

type total_order (a:eqtype) (f: (a -> a -> Tot bool)) =
   (forall a1 a2. (f a1 a2 /\ f a2 a1)  ==> a1 = a2)  (* anti-symmetry *)
 /\ (forall a1 a2 a3. f a1 a2 /\ f a2 a3 ==> f a1 a3)  (* transitivity  *)
 /\ (forall a1 a2. f a1 a2 \/ f a2 a1)                (* totality      *)

let string_cmp' s1 s2 =  String.compare s1 s2 <= 0

(* The F* defn just calls down to OCaml, since we know comparison in OCaml is total
 * just admit it
 *)
val string_cmp_total : unit -> Lemma (total_order string string_cmp')
let string_cmp_total () = admit ()

// hack function so, the data structure doesn't forget the proof!
val string_cmp : Map.cmp string
let string_cmp = string_cmp_total (); string_cmp'

(*******************************************************************)
(* Boiler plate over *)
(*******************************************************************)

type address = string

type accounts = Map.ordmap address nat string_cmp

// val add_account_values : Map.ordmap address int string_cmp -> int
// let add_account_values accounts = MapProp.fold (fun _k v acc -> v + acc) accounts 0

// we have to specify they are nats :(
val add_account_values : Map.ordmap address nat string_cmp -> nat
let add_account_values accounts =
 MapProp.fold (fun _key (v : nat) (acc : nat) -> v + acc) accounts 0

unopteq type storage = {
  total_supply : nat;
  accounts     : a : Map.ordmap address nat string_cmp { total_supply = add_account_values a }
}

val emtpy_storage : storage
let empty_storage = {
  total_supply = 0;
  accounts = Map.empty;
}
