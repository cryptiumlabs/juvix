module Sexp (top) where

import qualified Data.Set as Set
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error)

top :: T.TestTree
top =
  T.testGroup
    "sexp pass tests:"
    [ condWorksAsExpected,
      ifWorksAsExpected,
      letWorksAsExpected,
      doWorksAsExpected,
      recordWorksAsExpected,
      sigandDefunWorksAsExpetcted,
      moduleExpandsAsExpected
    ]

--------------------------------------------------------------------------------
-- Tests that will stay in this dir
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Translation Desugar Passes
--------------------------------------------------------------------------------

moduleTransform :: Sexp.T -> Sexp.T
moduleTransform xs = Sexp.foldPred xs (== "defmodule") moduleToRecord
  where
    moduleToRecord atom (name Sexp.:> args Sexp.:> body) =
      Sexp.list [Sexp.atom "defun", name, args, Sexp.foldr combine generatedRecord body]
        |> Sexp.addMetaToCar atom
      where
        generatedRecord =
          Sexp.list (Sexp.atom "record" : fmap (\x -> Sexp.list [Sexp.Atom x]) names)
        combine (form Sexp.:> name Sexp.:> xs) expression
          | Sexp.isAtomNamed form "defun" =
            -- we crunch the xs in a list
            Sexp.list [Sexp.atom "let", name, xs, expression]
          | Sexp.isAtomNamed form "type" =
            Sexp.list [Sexp.atom "let-type", name, xs, expression]
        combine (form Sexp.:> name Sexp.:> xs Sexp.:> Sexp.Nil) expression
          | Sexp.isAtomNamed form "defsig" =
            Sexp.list [Sexp.atom "let-sig", name, xs, expression]
        combine (form Sexp.:> declaration) expression
          | Sexp.isAtomNamed form "declare" =
            Sexp.list [Sexp.atom "declaim", declaration, expression]
        combine (Sexp.List [form, open]) expression
          | Sexp.isAtomNamed form "open" =
            Sexp.list [Sexp.atom "open-in", open, expression]
        combine (form Sexp.:> xs) expression
          | Sexp.isAtomNamed form "defmodule",
            Just atom <- Sexp.atomFromT form =
            -- have to recurse by hand here ☹
            let (_ Sexp.:> name Sexp.:> rest) = moduleToRecord atom xs
             in Sexp.list [Sexp.atom "let", name, rest, expression]
        -- ignore other forms
        combine _ expression = expression
        --
        names = Sexp.foldr f [] body |> Set.fromList |> Set.toList
        --
        f (form Sexp.:> name Sexp.:> _) acc
          | Sexp.isAtomNamed form "defun"
              || Sexp.isAtomNamed form "type"
              || Sexp.isAtomNamed form "defmodule"
              || Sexp.isAtomNamed form "defsig",
            Just name <- Sexp.atomFromT name =
            name : acc
        f _ acc = acc
    moduleToRecord _ _ = error "malformed record"

condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== "cond") condToIf
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

-- This one and sig combining are odd mans out, as they happen on a
-- list of transforms
-- We will get rid of this as this should be the job of Code -> Context!
multipleTransDefun :: [Sexp.T] -> [Sexp.T]
multipleTransDefun = search
  where
    combineMultiple name xs =
      Sexp.list ([Sexp.atom "defun-match", Sexp.atom name] <> (Sexp.cdr . Sexp.cdr <$> xs))
    sameName name (Sexp.List (defun1 : name1 : _))
      | Sexp.isAtomNamed defun1 "defun" && Sexp.isAtomNamed name1 name =
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
    search (defun@(Sexp.List (defun1 : name1@(Sexp.Atom a) : _)) : xs)
      | Sexp.isAtomNamed defun1 "defun",
        Just name <- Sexp.nameFromT name1 =
        let (sameDefun, toSearch) = grabSimilar name xs
         in combineMultiple name (defun : sameDefun)
              |> Sexp.addMetaToCar a
              |> (: search toSearch)
    search (x : xs) = x : search xs
    search [] = []

-- This pass will also be removed, but is here for comparability
-- reasons we just drop sigs with no defuns for now ☹. Fix this up when
-- we remove this pass
combineSig :: [Sexp.T] -> [Sexp.T]
combineSig
  ( Sexp.List [Sexp.Atom (Sexp.A "defsig" _), name, sig] :
      (Sexp.Atom a@(Sexp.A "defun-match" _) Sexp.:> defName Sexp.:> body) :
      xs
    )
    | defName == name =
      Sexp.addMetaToCar a (Sexp.listStar [Sexp.atom "defsig-match", name, sig, body])
        : combineSig xs
combineSig (Sexp.List [Sexp.Atom (Sexp.A "defsig" _), _, _] : xs) =
  combineSig xs
combineSig (x : xs) = x : combineSig xs
combineSig [] = []

multipleTransLet :: Sexp.T -> Sexp.T
multipleTransLet xs = Sexp.foldPred xs (== "let") letToLetMatch
  where
    letToLetMatch atom (Sexp.List [a@(Sexp.Atom (Sexp.A name _)), bindingsBody, rest]) =
      let (grabbed, notMatched) = grabSimilar name rest
       in Sexp.list
            [ Sexp.atom "let-match",
              a,
              putTogetherSplices (bindingsBody : grabbed),
              notMatched
            ]
            |> Sexp.addMetaToCar atom
    letToLetMatch _atom _ =
      error "malformed let"
    --
    grabSimilar name (Sexp.List [let1, name1, bindingsBody, rest])
      | Sexp.isAtomNamed let1 "let" && Sexp.isAtomNamed name1 name =
        grabSimilar name rest
          |> first (bindingsBody :)
    grabSimilar _name xs = ([], xs)
    --
    putTogetherSplices =
      foldr spliceBindingBody Sexp.Nil
    --
    spliceBindingBody (Sexp.List [bindings, body]) acc =
      Sexp.Cons bindings (Sexp.Cons body acc)
    spliceBindingBody _ _ =
      error "doesn't happen"

translateDo :: Sexp.T -> Sexp.T
translateDo xs = Sexp.foldPred xs (== "do") doToBind
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
removePunnedRecords xs = Sexp.foldPred xs (== "record") removePunned
  where
    removePunned atom sexp =
      Sexp.listStar
        [ Sexp.atom "record-no-pun",
          Sexp.foldr f Sexp.Nil sexp
        ]
        |> Sexp.addMetaToCar atom
      where
        f (Sexp.List [field, bind]) acc =
          field Sexp.:> bind Sexp.:> acc
        f (Sexp.List [pun]) acc =
          pun Sexp.:> pun Sexp.:> acc
        f _ _ = error "malformed record"

--------------------------------------------------------------------------------
-- Pass Tests
--------------------------------------------------------------------------------

condWorksAsExpected :: T.TestTree
condWorksAsExpected =
  T.testCase
    "cond properly desguars cond"
    (Sexp.parse expected T.@=? Right (condTransform testData))
  where
    expected =
      "(if (g x) true (if (p x) true (if else false)))"

ifWorksAsExpected :: T.TestTree
ifWorksAsExpected =
  T.testCase
    "if expansion to match works properly"
    (Sexp.parse expected T.@=? Right (ifTransform (condTransform testData)))
  where
    expected =
      "(case (g x) \
      \  (true true) \
      \  (false (case (p x) \
      \            (true true) \
      \            (false (case else (true false))))))"

letWorksAsExpected :: T.TestTree
letWorksAsExpected =
  T.testCase
    "let expansion to match works properly"
    (Sexp.parse expected T.@=? Right (multipleTransLet testLet))
  where
    expected =
      "(let-match foo ((Nil b) body-1 ((Cons a xs) b) body-2) \
      \   (let-match bar (((Cons a xs) b) body-2) \
      \     &rest))"

doWorksAsExpected :: T.TestTree
doWorksAsExpected =
  T.testCase
    "do expansion works propelry"
    (Sexp.parse expected T.@=? Right (translateDo testDo))
  where
    expected =
      "(Prelude.>>= computation \
      \    (lambda (x) \
      \       (Prelude.>> computation \
      \                   (Prelude.>>= more-comp \
      \                                (lambda (y) \
      \                                   (Prelude.return (+ x y)))))))"

recordWorksAsExpected :: T.TestTree
recordWorksAsExpected =
  T.testCase
    "removing punned names works as expected"
    (Sexp.parse expected T.@=? Right (removePunnedRecords testRecord))
  where
    expected =
      "(record-no-pun name value field-pun field-pun name2 value2)"

sigandDefunWorksAsExpetcted :: T.TestTree
sigandDefunWorksAsExpetcted =
  T.testCase
    "desugaring defuns and sigs work as epected"
    (expected T.@=? (multipleTransDefun testDefun |> combineSig))
  where
    expected =
      [ "(defsig-match f (-> a b) (((Cons a as) b) b1) ((Nil b) b2))",
        "(defun-match g ((a b) new-b))"
      ]
        >>| Sexp.parse
        >>| rightErr

moduleExpandsAsExpected :: T.TestTree
moduleExpandsAsExpected =
  T.testCase
    "module properly goes to record"
    (Sexp.parse expected T.@=? Right (moduleTransform moduleTest))
  where
    expected =
      "(defun fun-name () \
      \   (let-sig f (-> a b) \
      \       (let f (((Cons a as) b) b1) \
      \          (let f ((Nil b) b2) \
      \             (record (f))))))"

--------------------------------------------------------------------------------
-- Pass Test Data
--------------------------------------------------------------------------------

moduleTest :: Sexp.T
moduleTest =
  Sexp.listStar
    ([Sexp.atom "defmodule", Sexp.atom "fun-name", Sexp.list []] <> testDefun)

testData :: Sexp.T
testData =
  rightErr $ Sexp.parse "(cond ((g x) true) ((p x) true) (else false))"

testDefun :: [Sexp.T]
testDefun =
  [ Sexp.parse "(defsig f (-> a b))",
    Sexp.parse "(defun f ((Cons a as) b) b1)",
    Sexp.parse "(defun f (Nil b) b2)",
    Sexp.parse "(defun g (a b) new-b)"
  ]
    >>| rightErr

testLet :: Sexp.T
testLet =
  "(let foo ((Nil b) body-1)\
  \   (let foo (((Cons a xs) b) body-2) \
  \      (let bar (((Cons a xs) b) body-2) &rest)))"
    |> Sexp.parse
    |> rightErr

testDo :: Sexp.T
testDo =
  "(do (%<- x computation) computation (%<- y more-comp) (Prelude.return (+ x y)))"
    |> Sexp.parse
    |> rightErr

testRecord :: Sexp.T
testRecord =
  "(record (name value) (field-pun) (name2 value2))"
    |> Sexp.parse
    |> rightErr

rightErr :: Either a p -> p
rightErr (Right r) = r
rightErr (Left _) = error "improper right"
