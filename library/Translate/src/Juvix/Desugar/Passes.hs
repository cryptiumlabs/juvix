-- | Passes contains a list of passes on the frontend syntax that can be
-- done with no extra information needed. Thus we export the following passes
--
-- - Removing Explicit Module declarations
-- - Removing Guards
-- - Conds ⟶ If ⟶ Match
-- - Combining signatures to functions
-- - Removing punned record arguments
module Juvix.Desugar.Passes
  ( moduleTransform,
    condTransform,
    ifTransform,
    multipleTransDefun,
    combineSig,
    multipleTransLet,
    removePunnedRecords,
    moduleLetTransform,
    handlerTransform,
  )
where

import Control.Lens hiding ((|>))
import qualified Data.Set as Set
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp
import qualified Juvix.Sexp.Structure as Structure
import Juvix.Sexp.Structure.Lens
import Prelude (error)

--------------------------------------------------------------------------------
-- Fully Translated
--------------------------------------------------------------------------------

------------------------------------------------------------
-- Cond Desugar Passes
------------------------------------------------------------

-- | @condTransform@ - CondTransform turns the cond form of the fronted
-- language into a series of ifs
-- - BNF input form:
--   + (:Cond (pred-1 result-1) … (pred-n result-n))
-- - BNF output form:
--   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.foldPred xs (== Structure.nameCond) condToIf
  where
    condToIf atom cdr
      | Just cond <- Structure.toCond (Sexp.Atom atom Sexp.:> cdr),
        Just last <- lastMay (cond ^. entailments) =
        let acc =
              Structure.IfNoElse (last ^. predicate) (last ^. answer)
                |> Structure.fromIfNoElse
         in foldr generation acc (initSafe (cond ^. entailments))
              |> Sexp.addMetaToCar atom
      | otherwise = error "malformed cond"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf

-- | @ifTransform@ - transforms a generated if form into a case
-- - BNF input form:
--   1. (if pred then else)
--   2. (if pred then)
-- - BNF output form:
--   1. (case pred ((True) then) ((False) else))
--   2. (case pred ((True) then))
-- - Note =case=, =then=, and =else= are literals
ifTransform :: Sexp.T -> Sexp.T
ifTransform xs = Sexp.foldPred xs (== Structure.nameIf) ifToCase
  where
    ifToCase atom cdr =
      ( case Structure.toIfFull (Sexp.Atom atom Sexp.:> cdr) of
          Just (Structure.Else ifThenElse) ->
            caseListElse
              (ifThenElse ^. predicate)
              (ifThenElse ^. conclusion)
              (ifThenElse ^. alternative)
          Just (Structure.NoElse ifThen) ->
            caseList (ifThen ^. predicate) (ifThen ^. conclusion)
          Nothing ->
            error "malformed if"
      )
        |> Structure.fromCase
        |> Sexp.addMetaToCar atom
    -- Bad these functions should be refactored into using the case transform
    caseList pred then' =
      Structure.Case
        pred
        [createDeconBody "True" then']
    caseListElse pred then' else' =
      Structure.Case
        pred
        [createDeconBody "True" then', createDeconBody "False" else']
    createDeconBody con entailment =
      Structure.DeconBody (Structure.matchConstructor (Sexp.atom con)) entailment

------------------------------------------------------------
-- Defun Transformation
------------------------------------------------------------

-- These transform function like things,
-- TODO ∷ re-use code more between the first 2 passes here
--

-- | @multipleTransLet@ - transforms multiple let forms
-- into a single let-match
--
-- - BNF input form:
--   + (let f (arg-match-11 … arg-match-1n)     body-1
--       (let f (arg-match-21 … arg-match-2n)   body-2
--         …
--         (let f (arg-match-n1 … arg-match-nn) body-n
--            rest)))
-- - BNF output form
--   + (:let-match f ((args-match-11 … args-match-1n) body-1
--                    (args-match-21 … args-match-2n) body-2
--                    …
--                    (args-match-n1 … args-match-nn) body-n)
--        rest)
-- - Note the f's are exactly the same name
multipleTransLet :: Sexp.T -> Sexp.T
multipleTransLet xs = Sexp.foldPred xs (== Structure.nameLet) letToLetMatch
  where
    letToLetMatch atom cdr =
      let currentForm = Sexp.Atom atom Sexp.:> cdr
       in case Structure.toLet currentForm of
            Just let' ->
              let (letPatternMatches, notMatched) =
                    grabSim (let' ^. name) currentForm
               in Structure.LetMatch (let' ^. name) letPatternMatches notMatched
                    |> Structure.fromLetMatch
                    |> Sexp.addMetaToCar atom
            Nothing -> error "malformed let"
    grabSim name xs =
      case grabSimilarBinding name Structure.toLet xs of
        Just (structure, let') ->
          grabSim name (let' ^. rest)
            |> first (structure :)
        Nothing ->
          ([], xs)

-- | @grabSimilarBindings@ grabs forms with the same name with a
-- transformation, and gives back an ArgBody structure and the matched
-- structure
grabSimilarBinding ::
  ( Eq a,
    HasName s a,
    HasArgs s Sexp.T,
    HasBody s Sexp.T
  ) =>
  a ->
  (t -> Maybe s) ->
  t ->
  Maybe (Structure.ArgBody, s)
grabSimilarBinding nameGiven transform form = transform form >>= f
  where
    f form
      | form ^. name == nameGiven =
        Just (Structure.ArgBody (form ^. args) (form ^. body), form)
      | otherwise = Nothing

-- This one and sig combining are odd mans out, as they happen on a
-- list of transforms
-- We will get rid of this as this should be the job of Code -> Context!

-- | @multipleTransDefun@ - trasnforms multiple defun forms into a
-- single defun match form
-- - Input BNF:
--   + (:defun f (arg-11 … arg-1n) body-1)
--     (:defun f (arg-21 … arg-2n) body-2)
--     …
--     (:defun f (arg-n1 … arg-nn) body-n)
-- - Output BNF:
--   + (:defun-match f
--       ((arg-11 … arg-1n) body-1)
--       ((arg-21 … arg-2n) body-2)
--       …
--       ((arg-n1 … arg-nn) body-n))
-- - Notes :: We could replace the out layer of ()'s with nothing to
--   reduce the numbers of ()'s
multipleTransDefun :: [Sexp.T] -> [Sexp.T]
multipleTransDefun = search
  where
    search ys@(defun : _)
      | Just form <- Structure.toDefun defun,
        -- we do this to get the meta information
        Just atom <- Sexp.atomFromT (form ^. name) =
        let (matchBody, toSearch) = grabSim (form ^. name) ys
         in Structure.DefunMatch (form ^. name) matchBody
              |> Structure.fromDefunMatch
              |> Sexp.addMetaToCar atom
              |> (: search toSearch)
    search (x : xs) = x : search xs
    search [] = []
    grabSim name (defn : xs) =
      case grabSimilarBinding name Structure.toDefun defn of
        Just (structure, _def') ->
          grabSim name xs |> first (structure :)
        Nothing ->
          ([], defn : xs)
    grabSim _name [] =
      ([], [])

-- grabSim name xs =
--   case Structure.toDefun ${1:T}

-- This pass will also be removed, but is here for comparability
-- reasons we just drop sigs with no defuns for now ☹. Fix this up when
-- we remove this pass

-- | @combineSig@ - combines a sig and a defun-match to form a sig-match
-- - Input BNF:
--   1. (:defsig f signature)
--      (:defun-match f
--        ((arg-n1 … arg-nn) body-1)
--        …
--        ((arg-n1 … arg-nn) body-n))
--   2. (:defun-match f
--        ((arg-n1 … arg-nn) body-1)
--        …
--        ((arg-n1 … arg-nn) body-n))
-- - Output BNF:
--   1. (:defsig-match f signature
--        ((arg-11 … arg-1n) body-1)
--         …
--        ((arg-n1 … arg-nn) body-n))
--   2. (:defsig-match f ()
--         ((arg-n1 … arg-nn) body-1)
--         …
--         ((arg-n1 … arg-nn) body-n))
combineSig :: [Sexp.T] -> [Sexp.T]
combineSig (a : match : xs)
  | Just signature' <- Structure.toSignature a,
    Just m <- Structure.toDefunMatch match,
    -- we do this to get the meta information
    Just atom <- Sexp.atomFromT (m ^. name),
    signature' ^. name == m ^. name =
    --
    Structure.DefunSigMatch (m ^. name) (signature' ^. sig) (m ^. args)
      |> Structure.fromDefunSigMatch
      |> Sexp.addMetaToCar atom
      |> (: combineSig xs)
combineSig (defun : xs)
  | Just def <- Structure.toDefunMatch defun,
    Just atom <- Sexp.atomFromT (def ^. name) =
    Structure.DefunSigMatch (def ^. name) Sexp.Nil (def ^. args)
      |> Structure.fromDefunSigMatch
      |> Sexp.addMetaToCar atom
      |> (: combineSig xs)
combineSig (sig : xs)
  | Just _sig <- Structure.toSignature sig =
    combineSig xs
combineSig (x : xs) = x : combineSig xs
combineSig [] = []

------------------------------------------------------------
-- Misc transformations
------------------------------------------------------------

-- | @removePunnedRecords@ - removes the record puns from the syntax to
-- have an uniform a-list
-- - BNF input:
--   + (:record (punned-1) (name-2 body-2) … (punned-n))
-- - BNF output:
--   + (:record-no-pun punned-1 punned-1 name-2 body-2 … punned-n punned-n)
removePunnedRecords :: Sexp.T -> Sexp.T
removePunnedRecords xs = Sexp.foldPred xs (== Structure.nameRecord) removePunned
  where
    removePunned atom cdr =
      case Structure.toRecord (Sexp.Atom atom Sexp.:> cdr) of
        Just record ->
          fmap f (record ^. value)
            |> Structure.RecordNoPunned
            |> Structure.fromRecordNoPunned
            |> Sexp.addMetaToCar atom
        Nothing -> error "malformed record"
      where
        f (Structure.Pun punned) =
          Structure.NotPunned (punned ^. name) (punned ^. name)
        f (Structure.NotPun notPunned) = notPunned

------------------------------------------------------------
-- Module Pass
------------------------------------------------------------

-- Update this two fold.
-- 1. remove the inner combine and make it global
-- 2. Find a way to handle the cond case

-- | @moduleTransform@ - transforms a module and top level statements
-- into expressions
-- - _Top Level Transformation_
--   + defun     ⟶ let
--   + type      ⟶ let-type
--   + defsig    ⟶ let-sig
--   + declare   ⟶ declaim
--   + open      ⟶ open-in
--   + defmodule ⟶ let-mod
-- - BNF Input Form
--   1. (:defmodule name (arg1 … argn) toplevel-1 … toplevel-n)
--   2. (:defmodule name (arg1 … argn)
--         (cond
--           (pred-1 toplevel-11 … toplevel-1n)
--           …
--           (pred-n toplevel-n1 … toplevel-nn)))
-- - BNF output form
--   1. (defun name (arg1 … argn)
--        (expression-1 (… expression-n …
--                        (record (toplevel-1-name) … (toplevel-n-name)))))
--   2. (defun name (arg1 … argn)
--        (cond
--           (pred-1 (expression-11
--                      (… expression-1n …
--                         (record (toplevel-11-name) … (toplevel-1n-name)))))
--           …
--           (pred-n (expression-n1
--                      (… expression-nn …
--                         (record (toplevel-n1-name) … (toplevel-nn-name)))))))
-- - Where Expression follows the Top level Transformation, and
--   <foo-name> is the name of foo
moduleTransform :: Sexp.T -> Sexp.T
moduleTransform xs = Sexp.foldPred xs (== Structure.nameDefModule) moduleToRecord
  where
    intoRecord body =
      Sexp.foldr combine (generatedRecord body) body
    moduleToRecord atom cdr
      | Just mod <- Structure.toDefModule (Sexp.Atom atom Sexp.:> cdr) =
        Structure.Defun (mod ^. name) (mod ^. args) (ignoreCond (mod ^. body) intoRecord)
          |> Structure.fromDefun
          |> Sexp.addMetaToCar atom
      | otherwise = error "malformed defmodule"

-- | @moduleLetTransform@ - See @moduleTransform@'s comment
moduleLetTransform :: Sexp.T -> Sexp.T
moduleLetTransform xs = Sexp.foldPred xs (== Structure.nameLetModule) moduleToRecord
  where
    combineIntoRecord body =
      Sexp.foldr combine (generatedRecord body) body
    moduleToRecord atom cdr
      | Just mod <- Structure.toLetModule (Sexp.Atom atom Sexp.:> cdr) =
        Structure.Let
          (mod ^. name)
          (mod ^. args)
          (ignoreCond (mod ^. body) combineIntoRecord)
          (mod ^. rest)
          |> Structure.fromLet
          |> Sexp.addMetaToCar atom
    moduleToRecord _ _ = error "malformed let-mod"

----------------------------------------
-- Module Helpers
----------------------------------------

-- | @combine@ - is the helper for transforming top level statements
-- into expressions
combine :: Sexp.T -> Sexp.T -> Sexp.T
combine form expression
  | Just defun <- Structure.toDefun form =
    Structure.Let (defun ^. name) (defun ^. args) (defun ^. body) expression
      |> Structure.fromLet
  | Just open <- Structure.toOpen form =
    Structure.OpenIn (open ^. name) expression
      |> Structure.fromOpenIn
  | Just declare <- Structure.toDeclare form =
    Structure.Declaim (declare ^. claim) expression
      |> Structure.fromDeclaim
  | Just typ <- Structure.toType form =
    Structure.LetType (typ ^. nameAndSig) (typ ^. args) (typ ^. body) expression
      |> Structure.fromLetType
  | Just signature' <- Structure.toSignature form =
    Structure.LetSignature (signature' ^. name) (signature' ^. sig) expression
      |> Structure.fromLetSignature
  -- TODO ∷ cleanup this generation, a bit
  | Just mod <- Structure.toDefModule form =
    let cond = Sexp.car (mod ^. body)
        newBody
          | Sexp.isAtomNamed cond Structure.nameCond = cond
          | otherwise = mod ^. body
     in Structure.LetModule (mod ^. name) (mod ^. args) newBody expression
          |> Structure.fromLetModule
combine _ expression = expression

-- | @ignoreCond@ gets past the annoying cond cells for modules
ignoreCond :: Sexp.T -> (Sexp.T -> Sexp.T) -> Sexp.T
ignoreCond (form Sexp.:> Sexp.Nil) trans
  | Just cond <- Structure.toCond form =
    Sexp.listStar [form, foldr comb Sexp.Nil (cond ^. entailments)]
  where
    comb predAns acc =
      Sexp.listStar
        [Sexp.list [predAns ^. predicate, trans (predAns ^. answer)], acc]
ignoreCond xs trans = trans xs

-- | @generatedRecord@ - record generation
generatedRecord :: Sexp.T -> Sexp.T
generatedRecord body =
  Structure.Record (fmap (Structure.Pun . Structure.Punned . Sexp.Atom) (names body))
    |> Structure.fromRecord

-- | @names@ - folding @grabNames@ that uniquifyies the result to
-- achieve an unique list
names :: Sexp.T -> [Sexp.Atom]
names body = Sexp.foldr grabNames [] body |> Set.fromList |> Set.toList

-- | @grabNames@ - responsible for grabbing the names out of top levels
grabNames :: Sexp.T -> [Sexp.Atom] -> [Sexp.Atom]
grabNames (form Sexp.:> name Sexp.:> _) acc
  | Sexp.isAtomNamed form Structure.nameDefun
      || Sexp.isAtomNamed form Structure.nameType
      || Sexp.isAtomNamed form Structure.nameDefModule
      || Sexp.isAtomNamed form Structure.nameSignature,
    Just name <- Sexp.atomFromT name =
    name : acc
  | Sexp.isAtomNamed form Structure.nameType,
    Just name <- Sexp.atomFromT (Sexp.car name) =
    name : acc
grabNames _ acc = acc

----------------------------------------
-- Handler Transform
----------------------------------------
handlerTransform :: Sexp.T -> Sexp.T
handlerTransform xs = Sexp.foldPred xs (== Structure.nameDefHandler) handTrans
  where
    handTrans atom cdr
      | Just mod <- Structure.toDefHandler (Sexp.Atom atom Sexp.:> cdr) =
        let (ret_, ops_) = filterRet (mod ^. ops)
         in Structure.LetHandler (mod ^. name) ops_ ret_
              |> Structure.fromLetHandler
              |> Sexp.addMetaToCar atom
    handTrans _ _ = error "malformed defhandler"

filterRet :: Sexp.T -> (Sexp.T, Sexp.T)
filterRet form = Sexp.foldr removeRet (Sexp.Nil, Sexp.Nil) form
  where
    removeRet form@(_ Sexp.:> name Sexp.:> _) (ret, acc)
      | Sexp.isAtomNamed name "pure" = (form, acc)
      | otherwise = (ret, form Sexp.:> acc)
    removeRet _ _ = error "can't happen"
