-- | Passes contains a list of passes on the frontend syntax that can be
-- done with no extra information needed. Thus we export the following passes
--
-- - Removing Explicit Module declarations
-- - Removing Guards
-- - Conds ⟶ If ⟶ Match
-- - Combining signatures to functions
-- - Removing punned record arguments
-- - Remvoing Do syntax
module Juvix.Desugar.Passes
  ( moduleTransform,
    condTransform,
    ifTransform,
    multipleTransDefun,
    combineSig,
    multipleTransLet,
    translateDo,
    removePunnedRecords,
    moduleLetTransform,
  )
where

import qualified Data.Set as Set
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

--------------------------------------------------------------------------------
-- Fully Translated
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Cond Desugar Passes
------------------------------------------------------------

condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== ":cond") condToIf
  where
    condToIf atom cdr =
      let acc =
            generation (Sexp.last cdr) Sexp.Nil
              |> Sexp.butLast
       in Sexp.foldr generation acc (Sexp.butLast cdr)
            |> Sexp.addMetaToCar atom
    --
    generation (Sexp.Cons condition body) acc =
      Sexp.list [Sexp.atom "if", condition, Sexp.car body, acc]
    generation _ _ =
      error "malformed cond"

ifTransform :: Sexp.T -> Sexp.T
ifTransform xs = Sexp.foldPred xs (== "if") ifToCase
  where
    ifToCase atom cdr =
      case cdr of
        Sexp.List [pred, then', else'] ->
          Sexp.list (caseListElse pred then' else')
        Sexp.List [pred, then'] ->
          Sexp.list (caseList pred then')
        _ ->
          error "malformed if"
            |> Sexp.addMetaToCar atom
    caseList pred then' =
      [Sexp.atom "case", pred, Sexp.list [Sexp.atom "true", then']]
    caseListElse pred then' else' =
      caseList pred then' <> [Sexp.list [Sexp.atom "false", else']]

------------------------------------------------------------
-- Defun Transformation
------------------------------------------------------------
-- These transform function like things,
-- TODO ∷ re-use code more between the first 2 passes here

multipleTransLet :: Sexp.T -> Sexp.T
multipleTransLet xs = Sexp.foldPred xs (== "let") letToLetMatch
  where
    letToLetMatch atom (Sexp.List [a@(Sexp.Atom (Sexp.A name _)), bindings, body, rest]) =
      let (grabbed, notMatched) = grabSimilar name rest
       in Sexp.list
            [ Sexp.atom "let-match",
              a,
              putTogetherSplices (Sexp.list [bindings, body] : grabbed),
              notMatched
            ]
            |> Sexp.addMetaToCar atom
    letToLetMatch _atom _ =
      error "malformed let"
    --
    grabSimilar name (Sexp.List [let1, name1, bindings, body, rest])
      | Sexp.isAtomNamed let1 "let" && Sexp.isAtomNamed name1 name =
        grabSimilar name rest
          |> first (Sexp.list [bindings, body] :)
    grabSimilar _name xs = ([], xs)
    --
    putTogetherSplices =
      foldr spliceBindingBody Sexp.Nil
    --
    spliceBindingBody (Sexp.List [bindings, body]) acc =
      Sexp.Cons bindings (Sexp.Cons body acc)
    spliceBindingBody _ _ =
      error "doesn't happen"

-- This one and sig combining are odd mans out, as they happen on a
-- list of transforms
-- We will get rid of this as this should be the job of Code -> Context!
multipleTransDefun :: [Sexp.T] -> [Sexp.T]
multipleTransDefun = search
  where
    search (defun@(Sexp.List (defun1 : name1@(Sexp.Atom a) : _)) : xs)
      | Sexp.isAtomNamed defun1 ":defun",
        Just name <- Sexp.nameFromT name1 =
        let (sameDefun, toSearch) = grabSimilar name xs
         in combineMultiple name (defun : sameDefun)
              |> Sexp.addMetaToCar a
              |> (: search toSearch)
    search (x : xs) = x : search xs
    search [] = []
    combineMultiple name xs =
      Sexp.list ([Sexp.atom ":defun-match", Sexp.atom name] <> (Sexp.cdr . Sexp.cdr <$> xs))
    sameName name (Sexp.List (defun1 : name1 : _))
      | Sexp.isAtomNamed defun1 ":defun" && Sexp.isAtomNamed name1 name =
        True
    sameName _ _ =
      False
    grabSimilar _nam [] = ([], [])
    grabSimilar name (defn : xs)
      | sameName name defn =
        let (same, rest) = grabSimilar name xs
         in (defn : same, rest)
      | otherwise =
        ([], defn : xs)

-- This pass will also be removed, but is here for comparability
-- reasons we just drop sigs with no defuns for now ☹. Fix this up when
-- we remove this pass
combineSig :: [Sexp.T] -> [Sexp.T]
combineSig
  ( Sexp.List [Sexp.Atom (Sexp.A ":defsig" _), name, sig] :
      (Sexp.Atom a@(Sexp.A ":defun-match" _) Sexp.:> defName Sexp.:> body) :
      xs
    )
    | defName == name =
      Sexp.addMetaToCar a (Sexp.listStar [Sexp.atom ":defsig-match", name, sig, body])
        : combineSig xs
combineSig (Sexp.List [Sexp.Atom a@(Sexp.A ":defun-match" _), defName, body] : xs) =
  Sexp.addMetaToCar a (Sexp.list [Sexp.atom ":defsig-match", defName, Sexp.Nil, body])
    : combineSig xs
combineSig (Sexp.List [Sexp.Atom (Sexp.A ":defsig" _), _, _] : xs) =
  combineSig xs
combineSig (x : xs) = x : combineSig xs
combineSig [] = []

------------------------------------------------------------
-- Misc transformations
------------------------------------------------------------

translateDo :: Sexp.T -> Sexp.T
translateDo xs = Sexp.foldPred xs (== ":do") doToBind
  where
    doToBind atom sexp =
      Sexp.foldr generation acc (Sexp.butLast sexp)
        |> Sexp.addMetaToCar atom
      where
        acc =
          case Sexp.last sexp of
            -- toss away last %<-... we should likely throw a warning for this
            Sexp.List [Sexp.Atom (Sexp.A "%<-" _), _name, body] -> body
            xs -> xs
        generation body acc =
          case body of
            Sexp.List [Sexp.Atom (Sexp.A "%<-" _), name, body] ->
              Sexp.list
                [ Sexp.atom "Prelude.>>=",
                  body,
                  Sexp.list [Sexp.atom "lambda", Sexp.list [name], acc]
                ]
            notBinding ->
              Sexp.list [Sexp.atom "Prelude.>>", notBinding, acc]

removePunnedRecords :: Sexp.T -> Sexp.T
removePunnedRecords xs = Sexp.foldPred xs (== ":record") removePunned
  where
    removePunned atom sexp =
      Sexp.listStar
        [ Sexp.atom ":record-no-pun",
          Sexp.foldr f Sexp.Nil sexp
        ]
        |> Sexp.addMetaToCar atom
      where
        f (Sexp.List [field, bind]) acc =
          field Sexp.:> bind Sexp.:> acc
        f (Sexp.List [pun]) acc =
          pun Sexp.:> pun Sexp.:> acc
        f _ _ = error "malformed record"

------------------------------------------------------------
-- Module Pass
------------------------------------------------------------

-- Update this two fold.
-- 1. remove the inner combine and make it global
-- 2. Find a way to handle the cond case
moduleTransform :: Sexp.T -> Sexp.T
moduleTransform xs = Sexp.foldPred xs (== ":defmodule") moduleToRecord
  where
    moduleToRecord atom (name Sexp.:> args Sexp.:> body) =
      Sexp.list
        [ Sexp.atom ":defun",
          name,
          args,
          ignoreCond body (\b -> Sexp.foldr combine (generatedRecord b) b)
        ]
        |> Sexp.addMetaToCar atom
    moduleToRecord _ _ = error "malformed record"

moduleLetTransform :: Sexp.T -> Sexp.T
moduleLetTransform xs = Sexp.foldPred xs (== ":let-mod") moduleToRecord
  where
    moduleToRecord atom (name Sexp.:> args Sexp.:> body Sexp.:> rest) =
      Sexp.list
        [ Sexp.atom "let",
          name,
          args,
          ignoreCond body (\b -> Sexp.foldr combine (generatedRecord b) b),
          rest
        ]
        |> Sexp.addMetaToCar atom
    moduleToRecord _ _ = error "malformed record"

----------------------------------------
-- Module Helpers
----------------------------------------

combine :: Sexp.T -> Sexp.T -> Sexp.T
combine (form Sexp.:> name Sexp.:> args Sexp.:> body Sexp.:> Sexp.Nil) expression
  | Sexp.isAtomNamed form ":defun" =
    -- we crunch the xs in a list
    Sexp.list [Sexp.atom "let", name, args, body, expression]
combine (form Sexp.:> name Sexp.:> xs) expression
  | Sexp.isAtomNamed form "type" =
    Sexp.list [Sexp.atom ":let-type", name, xs, expression]
combine (form Sexp.:> name Sexp.:> xs Sexp.:> Sexp.Nil) expression
  | Sexp.isAtomNamed form ":defsig" =
    Sexp.list [Sexp.atom ":let-sig", name, xs, expression]
combine (form Sexp.:> declaration) expression
  | Sexp.isAtomNamed form "declare" =
    Sexp.list [Sexp.atom ":declaim", declaration, expression]
combine (Sexp.List [form, open]) expression
  | Sexp.isAtomNamed form "open" =
    Sexp.list [Sexp.atom ":open-in", open, expression]
combine (form Sexp.:> name Sexp.:> args Sexp.:> xs) expression
  | Sexp.isAtomNamed form ":defmodule" =
    -- Turn this into a let-module
    if  | Sexp.isAtomNamed (Sexp.car xs) ":cond" ->
          Sexp.list [Sexp.atom ":let-mod", name, args, Sexp.car xs, expression]
        | otherwise ->
          Sexp.list [Sexp.atom ":let-mod", name, args, xs, expression]
-- ignore other forms
combine _ expression = expression

ignoreCond :: Sexp.T -> (Sexp.T -> Sexp.T) -> Sexp.T
ignoreCond ((form Sexp.:> xs) Sexp.:> Sexp.Nil) trans
  | Sexp.isAtomNamed form ":cond" =
    Sexp.listStar [form, Sexp.foldr comb Sexp.Nil xs]
  where
    comb (pred Sexp.:> body) acc =
      Sexp.listStar [Sexp.list [pred, trans body], acc]
    comb _ acc = acc
ignoreCond xs trans = trans xs

generatedRecord :: Sexp.T -> Sexp.T
generatedRecord b =
  Sexp.list
    (Sexp.atom ":record" : fmap (\x -> Sexp.list [Sexp.Atom x]) (names b))

names :: Sexp.T -> [Sexp.Atom]
names body = Sexp.foldr grabNames [] body |> Set.fromList |> Set.toList

grabNames :: Sexp.T -> [Sexp.Atom] -> [Sexp.Atom]
grabNames (form Sexp.:> name Sexp.:> _) acc
  | Sexp.isAtomNamed form ":defun"
      || Sexp.isAtomNamed form "type"
      || Sexp.isAtomNamed form ":defmodule"
      || Sexp.isAtomNamed form ":defsig",
    Just name <- Sexp.atomFromT name =
    name : acc
  | Sexp.isAtomNamed (Sexp.car name) "type",
    Just name <- Sexp.atomFromT (Sexp.car name) =
    name : acc
grabNames _ acc = acc
