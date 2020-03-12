{-# LANGUAGE ApplicativeDo #-}

-- |
-- - The front end parser for the Juvix Programming language
--
-- - Parsers with S at the end, eat the spaces at the end of the parse
--
-- - Parsers with SN at the end, eats the spaces and new lines at the
--   end of the parse
module Juvix.Frontend.Parser where

import Data.Attoparsec.ByteString hiding (match)
import qualified Data.Attoparsec.Expr as Expr
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Encoding
import qualified Juvix.Core.Usage as Usage
import qualified Juvix.Frontend.Lexer as Lexer
import qualified Juvix.Frontend.Types as Types
import Juvix.Library hiding (guard, maybe, option, product, sum, take, takeWhile, try)
import Prelude (fail)

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

topLevel ∷ Parser Types.TopLevel
topLevel =
  Types.Type <$> typeP
    <|> Types.ModuleOpen <$> moduleOpen
    <|> Types.Module <$> module'
    <|> Types.Function <$> function
    <|> Types.Signature <$> signature'

expressionGen' ∷ Parser Types.Expression → Parser Types.Expression
expressionGen' p =
  Types.Cond <$> cond
    <|> Types.Let <$> let'
    <|> Types.Match <$> match
    <|> Types.OpenExpr <$> moduleOpenExpr
    <|> Types.Block <$> block
    <|> Types.Lambda <$> lam
    <|> Types.ExpRecord <$> expRecord
    <|> Types.Application <$> try application
    <|> try p
    -- <|> Types.String <$> string'
    -- unfinished
    <|> Types.Constant <$> constant
    <|> Types.Name <$> prefixSymbolDot
    <|> parens (expressionGen p)

expressionGen ∷ Parser Types.Expression → Parser Types.Expression
expressionGen p =
  Expr.buildExpressionParser tableExp (spaceLiner (expressionGen' p))

expression' ∷ Parser Types.Expression
expression' = expressionGen (fail "")

expression ∷ Parser Types.Expression
expression = expressionGen (Types.Do <$> do')

usage ∷ Parser Types.Expression
usage = string "u#" *> expression

--------------------------------------------------------------------------------
-- Modules/ Function Gen
--------------------------------------------------------------------------------

functionModGen ∷ Parser a → Parser (Types.FunctionLike a)
functionModGen p = do
  -- for now
  _ ← spaceLiner (string "let")
  name ← prefixSymbolSN
  args ← many argSN
  guard ← guard p
  pure (Types.Like name args guard)

--------------------------------------------------
-- Guard
--------------------------------------------------

guard ∷ Parser a → Parser (Types.GuardBody a)
guard p =
  Types.Guard <$> condB p
    <|> Types.Body <$> (skipLiner Lexer.equals *> p)

--------------------------------------------------
-- Args
--------------------------------------------------

arg ∷ Parser Types.Arg
arg =
  Types.ImplicitA <$> (skip (== Lexer.hash) *> matchLogic)
    <|> Types.ConcreteA <$> matchLogic

--------------------------------------------------------------------------------
-- Signature
--------------------------------------------------------------------------------

signature' ∷ Parser Types.Signature
signature' = do
  _ ← spaceLiner (string "sig")
  name ← prefixSymbolSN
  maybeUsage ← maybe expressionSN
  skipLiner Lexer.colon
  typeclasses ← signatureConstraintSN
  arrowType ← arrowType
  pure (Types.Sig name maybeUsage arrowType typeclasses)

signatureConstraint ∷ Parser [Types.TypeName]
signatureConstraint =
  pure <$> typeNameParserSN <* string "=>"
    <|> parens (sepBy typeNameParserSN (skipLiner Lexer.comma)) <* string "=>"
    <|> pure []

--------------------------------------------------------------------------------
-- Match
--------------------------------------------------------------------------------

match ∷ Parser Types.Match
match = do
  _ ← spaceLiner (string "case")
  matchOn ← expressionSN
  _ ← spaceLiner (string "of")
  matchs ← many1H matchLSN
  pure (Types.Match' matchOn matchs)

matchL ∷ Parser Types.MatchL
matchL = do
  skipLiner Lexer.pipe
  match ← matchLogicSN
  spaceLiner (string "->")
  exp ← expression
  pure (Types.MatchL match exp)

matchLogic ∷ Parser Types.MatchLogic
matchLogic = maybeParend (matchLogicNamedSN <|> matchLogicNotNamedSN)

matchLogicNamed ∷ Parser Types.MatchLogic
matchLogicNamed = do
  name ← prefixSymbolDot
  skipLiner Lexer.at
  start ← maybeParend matchLogicStartSN
  pure (Types.MatchLogic start (Just name))

matchLogicNotNamed ∷ Parser Types.MatchLogic
matchLogicNotNamed = do
  start ← matchLogicStart
  pure (Types.MatchLogic start Nothing)

matchLogicStart ∷ Parser Types.MatchLogicStart
matchLogicStart = matchRecord <|> matchCon <|> matchName <|> matchConstant

matchConstant ∷ Parser Types.MatchLogicStart
matchConstant = Types.MatchConst <$> constant

matchCon ∷ Parser Types.MatchLogicStart
matchCon = do
  con ← prefixCapitalDotSN
  matchd ← many matchLogicSN
  pure (Types.MatchCon con matchd)

matchName ∷ Parser Types.MatchLogicStart
matchName = Types.MatchName <$> prefixSymbol

matchRecord ∷ Parser Types.MatchLogicStart
matchRecord =
  Types.MatchRecord <$> nameSetMany matchLogic

--------------------------------------------------------------------------------
-- NameSet
--------------------------------------------------------------------------------

nameSetMany ∷ Parser a → Parser (NonEmpty (Types.NameSet a))
nameSetMany parser =
  curly (sepBy1HFinal (nameSetSN parser) (skipLiner Lexer.comma))

nameSet ∷ Parser a → Parser (Types.NameSet a)
nameSet parser = nameMatch parser <|> namePunned

namePunned ∷ Parser (Types.NameSet a)
namePunned = Types.Punned <$> prefixSymbolDot

nameMatch ∷ Parser a → Parser (Types.NameSet a)
nameMatch parser = do
  name ← prefixSymbolDotSN
  skipLiner Lexer.equals
  bound ← parser
  pure (Types.NonPunned name bound)

--------------------------------------------------------------------------------
-- Function
--------------------------------------------------------------------------------

function ∷ Parser Types.Function
function = Types.Func <$> functionModGen expression

--------------------------------------------------------------------------------
-- Modules
--------------------------------------------------------------------------------

module' ∷ Parser Types.Module
module' = Types.Mod <$> functionModGen (many1H topLevelSN) <* string "end"

moduleOpen ∷ Parser Types.ModuleOpen
moduleOpen = do
  _ ← spaceLiner (string "open")
  Types.Open <$> moduleName

moduleName ∷ Parser Types.ModuleName
moduleName =
  prefixSymbolDot

moduleOpenExpr ∷ Parser Types.ModuleOpenExpr
moduleOpenExpr = moduleOpenExprNormal <|> moduleOpenExprParens

moduleOpenExprParens ∷ Parser Types.ModuleOpenExpr
moduleOpenExprParens = do
  name ← moduleName
  word8 Lexer.dot
  expr ← parens expression
  pure (Types.OpenExpress name expr)

moduleOpenExprNormal ∷ Parser Types.ModuleOpenExpr
moduleOpenExprNormal = do
  _ ← spaceLiner (string "open")
  name ← moduleNameSN
  _ ← spaceLiner (string "in")
  expr ← expression
  pure (Types.OpenExpress name expr)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

typeP ∷ Parser Types.Type
typeP = do
  _ ← spaceLiner (string "type")
  usag ← maybe usage
  name ← prefixSymbolSN
  args ← many prefixSymbolSN
  form ← typeSumParser
  pure (Types.Typ usag name args form)

typeSumParser ∷ Parser Types.TypeSum
typeSumParser =
  Types.NewType <$> try newTypeParser
    <|> Types.Alias <$> try aliasParser
    <|> Types.Data <$> dataParser

newTypeParser ∷ Parser Types.NewType
newTypeParser = do
  let nonOverlappingCase = do
        p ← peekWord8
        case p of
          Just p
            | p == Lexer.dash || p == Lexer.colon →
              fail "overlapping"
            | otherwise → pure ()
          Nothing → pure ()
  skipLiner Lexer.equals
  skipLiner Lexer.pipe
  -- if we get a | or a - at the end of this, then we need to go to the other case
  -- Note that we may end up with a non boxed type, but that is fine
  -- this is a subset of the ADT case for analysis
  Types.Declare <$> prefixSymbolSN <*> typeRefineSN
    <* nonOverlappingCase

aliasParser ∷ Parser Types.Alias
aliasParser = do
  skipLiner Lexer.equals
  Types.AliasDec <$> typeRefine

dataParser ∷ Parser Types.Data
dataParser = do
  arrow ← maybe (skipLiner Lexer.colon *> arrowTypeSN)
  skipLiner Lexer.equals
  adt ← adt
  case arrow of
    Just arr → pure (Types.Arrowed arr adt)
    Nothing → pure (Types.NonArrowed adt)

--------------------------------------------------
-- ADT parser
--------------------------------------------------

adt ∷ Parser Types.Adt
adt =
  Types.Sum <$> many1H sumSN
    <|> Types.Product <$> product

sum ∷ Parser Types.Sum
sum = do
  skipLiner Lexer.pipe
  Types.S <$> prefixSymbolSN <*> maybe product

product ∷ Parser Types.Product
product =
  Types.Record <$> record
    <|> Types.Arrow <$> arrowType

record ∷ Parser Types.Record
record = do
  names ←
    spaceLiner
      $ curly
      $ sepBy1HFinal nameTypeSN (skipLiner Lexer.comma)
  familySignature ← maybe (skipLiner Lexer.colon *> typeRefine)
  pure (Types.Record' names familySignature)

nameType ∷ Parser Types.NameType
nameType = do
  name ← nameParserSN
  skipLiner Lexer.colon
  sig ← arrowType
  pure (Types.NameType sig name)

nameParserColon ∷ Parser Types.Name
nameParserColon =
  nameParserSN <* skip (== Lexer.colon)

nameParser ∷ Parser Types.Name
nameParser =
  (skip (== Lexer.hash) *> fmap Types.Implicit prefixSymbol)
    <|> Types.Concrete <$> prefixSymbol

--------------------------------------------------
-- Arrow Type parser
--------------------------------------------------

-- This family of parsers are a bit complicated
-- Though they work well!

arrowType ∷ Parser Types.ArrowType
arrowType = do
  let toType (Arrow a) = Types.Arrows a
      toType (Parens p) = Types.Parens p
  recursive ← arrowOrParens
  end ← fmap Types.Refined (namedRefine <|> parens namedRefine) <|> endArrow
  pure (foldr toType end recursive)

data ArrowOrParens
  = Arrow Types.ArrowData
  | Parens Types.ArrowParen
  deriving (Show)

endArrow ∷ Parser Types.ArrowType
endArrow = Types.End <$> parens arrowType

arrowOrParens ∷ Parser [ArrowOrParens]
arrowOrParens =
  many (Arrow <$> arrowsSN <|> Parens <$> parendArrowSN)

arrowGen ∷ Parser a → Parser (Types.ArrowGen a)
arrowGen p =
  Types.ArrGen <$> maybe nameParserColonSN <*> p <*> arrowSymbol

arrows ∷ Parser Types.ArrowData
arrows =
  Types.Arr <$> arrowGen typeRefineSN

parendArrow ∷ Parser Types.ArrowParen
parendArrow =
  Types.Paren <$> parens (arrowGen arrowTypeSN)

namedRefine ∷ Parser Types.NamedRefine
namedRefine =
  Types.NamedRefine <$> maybe nameParserColonSN <*> typeRefine

arrowSymbol ∷ Parser Types.ArrowSymbol
arrowSymbol =
  Types.ArrowUse Usage.Omega <$ string "->"
    <|> Types.ArrowUse one <$ string "-o"
    <|> Types.ArrowUse mempty <$ string "-|"
    <|> ( do
            skipLiner Lexer.dash
            exp ← expressionSN
            string "->"
            pure (Types.ArrowExp exp)
        )

--------------------------------------------------
-- TypeNameParser and typeRefine Parser
--------------------------------------------------

typeRefine ∷ Parser Types.TypeRefine
typeRefine = do
  typeName ← typeNameParserSN
  refine ← maybe (curly expressionSN)
  pure (Types.TypeRefine typeName refine)

typeNameParser ∷ Parser Types.TypeName
typeNameParser = do
  pre ← prefixSymbolDotSN
  body ←
    many
      $ spaceLiner
      $ universeSymbol
        <|> Types.SymbolName <$> prefixSymbolDotSN
        <|> Types.ArrowName <$> parens arrowTypeSN
  pure (Types.Start pre body)

universeSymbol ∷ Parser Types.TypeNameValid
universeSymbol = do
  _ ← string "u#"
  Types.UniverseName <$> universeExpression

universeExpression ∷ Parser Types.UniverseExpression
universeExpression =
  Types.UniverseExpression <$> prefixSymbolSN
    -- TODO ∷ make this proper do + and max!
    <|> Types.UniverseExpression <$> parens prefixSymbolSN

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

block ∷ Parser Types.Block
block = do
  _ ← spaceLiner (string "begin")
  exp ← expressionSN
  _ ← string "end"
  pure (Types.Bloc exp)

--------------------------------------------------
-- Records
--------------------------------------------------

expRecord ∷ Parser Types.ExpRecord
expRecord = Types.ExpressionRecord <$> nameSetMany expression

--------------------------------------------------
-- Let
--------------------------------------------------

let' ∷ Parser Types.Let
let' = do
  spaceLiner (string "let")
  binds ← many1H bindingSN
  spaceLiner (string "in")
  body ← expression
  pure (Types.Let' binds body)

binding ∷ Parser Types.Binding
binding = do
  pattern' ← matchLogicSN
  skipLiner Lexer.equals
  on ← expression
  pure (Types.Bind pattern' on)

--------------------------------------------------
-- Cond
--------------------------------------------------

cond ∷ Parser (Types.Cond Types.Expression)
cond = do
  _ ← spaceLiner (string "if")
  condB expression

condB ∷ Parser a → Parser (Types.Cond a)
condB p = Types.C <$> many1H (condLogicSN p)

condLogic ∷ Parser a → Parser (Types.CondLogic a)
condLogic p = do
  skipLiner Lexer.pipe
  pred ← expressionSN
  skipLiner Lexer.equals
  body ← p
  pure (Types.CondExpression pred body)

--------------------------------------------------
-- Lambda
--------------------------------------------------

lam ∷ Parser Types.Lambda
lam = do
  skipLiner Lexer.backSlash
  args ← many1H matchLogicSN
  _ ← spaceLiner (string "->")
  body ← expression
  pure (Types.Lamb args body)

--------------------------------------------------
-- Application
--------------------------------------------------

application ∷ Parser Types.Application
application = do
  name ← prefixSymbolDotSN
  args ← many1H expressionSN
  pure (Types.App name args)

--------------------------------------------------
-- Constants
--------------------------------------------------

constant ∷ Parser Types.Constant
constant = Types.Number <$> number <|> Types.String <$> string'

number ∷ Parser Types.Numb
number =
  Types.Integer' <$> integer
    <|> Types.Double' <$> float

integer ∷ Parser Integer
integer = do
  digits ← takeWhile Lexer.digit
  case Char8.readInteger digits of
    Just (x, _) → pure x
    Nothing → fail "didn't parse an int"

float ∷ Parser Double
float = do
  _s1 ← takeWhile Lexer.digit
  skip (== Lexer.dot)
  _s2 ← takeWhile Lexer.digit
  fail "float not implemented"

--   pure (read (s1 <> "." <> s2))

string' ∷ Parser Types.String'
string' = do
  word8 Lexer.quote
  words ← takeWhile (/= Lexer.quote)
  word8 Lexer.quote
  pure (Types.Sho (Encoding.decodeUtf8 words))

--------------------------------------------------
-- Do
--------------------------------------------------

do' ∷ Parser Types.Do
do' = do
  doExp ← do''
  case length doExp of
    1 → fail "do expression with only 1 value"
    0 → fail "parser faled with empty list"
    _ → pure (Types.Do' $ NonEmpty.fromList doExp)

doBind ∷ Parser [Types.DoBody]
doBind = do
  name ← prefixSymbolDotSN
  spaceLiner (string "<-")
  body ← expression'SN
  pure [Types.DoBody (Just name) body]

doNotBind ∷ Parser [Types.DoBody]
doNotBind = do
  body ← expression'SN
  pure [Types.DoBody Nothing body]

do'' ∷ Parser [Types.DoBody]
do'' = Expr.buildExpressionParser table (doBind <|> doNotBind) <?> "bind expr"

--------------------------------------------------------------------------------
-- Symbol Handlers
--------------------------------------------------------------------------------

infixSymbolGen ∷ Parser Symbol → Parser Symbol
infixSymbolGen p = do
  symb ← p
  if
    | Set.member symb reservedSymbols → fail "symbol is reserved word"
    | otherwise → pure symb

infixSymbolDot ∷ Parser (NonEmpty Symbol)
infixSymbolDot = do
  qualified ← option [] (NonEmpty.toList <$> prefixSymbolDot <* word8 Lexer.dot)
  infix' ← infixSymbol
  pure (NonEmpty.fromList (qualified <> [infix']))

infixSymbol ∷ Parser Symbol
infixSymbol = infixSymbolGen (infixSymbol' <|> infixPrefix)

infixSymbol' ∷ Parser Symbol
infixSymbol' = internText . Encoding.decodeUtf8 <$> takeWhile Lexer.validInfixSymbol

infixPrefix ∷ Parser Symbol
infixPrefix =
  word8 Lexer.backtick *> prefixSymbol <* word8 Lexer.backtick

prefixSymbolGen ∷ Parser Word8 → Parser Symbol
prefixSymbolGen startParser = do
  start ← startParser
  rest ← takeWhile Lexer.validMiddleSymbol
  -- Slow O(n) call, could maybe peek ahead instead, then parse it all at once?
  let new = ByteString.cons start rest
  if
    | Set.member new reservedWords → fail "symbol is reserved operator"
    | otherwise → pure (internText (Encoding.decodeUtf8 new))

-- TODO ∷ this may be bad
-- this allows "(*).Foo.(<*>)" to be accepted
-- Though Should we allow this since, these are prefix if spelled this way
-- we don't enforce capitalization, and thus it would be improper for to
-- special case it out!
prefixSymbolDot ∷ Parser (NonEmpty Symbol)
prefixSymbolDot = sepBy1H prefixSymbol (word8 Lexer.dot)

prefixCapitalDot ∷ Parser (NonEmpty Symbol)
prefixCapitalDot = sepBy1H prefixCapital (word8 Lexer.dot)

prefixSymbol ∷ Parser Symbol
prefixSymbol =
  prefixSymbolGen (satisfy Lexer.validStartSymbol)
    <|> parend

parend ∷ Parser Symbol
parend = word8 Lexer.openParen *> infixSymbolGen infixSymbol' <* word8 Lexer.closeParen

prefixCapital ∷ Parser Symbol
prefixCapital = prefixSymbolGen (satisfy Lexer.validUpperSymbol)

--------------------------------------------------------------------------------
-- Misc helpers
--------------------------------------------------------------------------------

reservedWords ∷ (Ord a, IsString a) ⇒ Set a
reservedWords =
  Set.fromList
    ["let", "val", "type", "case", "in", "open", "if", "cond", "end", "of", "begin", "sig"]

reservedSymbols ∷ (Ord a, IsString a) ⇒ Set a
reservedSymbols =
  Set.fromList
    ["=", "|", ""]

maybe ∷ Alternative f ⇒ f a → f (Maybe a)
maybe = optional

spacer ∷ Parser p → Parser p
spacer p = p <* takeWhile (Lexer.space ==)

spaceLiner ∷ Parser p → Parser p
spaceLiner p = p <* takeWhile (\x → Lexer.space == x || Lexer.endOfLine x)

between ∷ Word8 → Parser p → Word8 → Parser p
between fst p end = skipLiner fst *> spaceLiner p <* satisfy (== end)

parens ∷ Parser p → Parser p
parens p = between Lexer.openParen p Lexer.closeParen

curly ∷ Parser p → Parser p
curly p = between Lexer.openCurly p Lexer.closeCurly

many1H ∷ Alternative f ⇒ f a → f (NonEmpty a)
many1H = fmap NonEmpty.fromList . many1

-- | 'sepBy1HFinal' is like 'sepBy1H' but also tries to parse a last separator
sepBy1HFinal ∷ Alternative f ⇒ f a → f s → f (NonEmpty a)
sepBy1HFinal parse sep = sepBy1H parse sep <* maybe sep

sepBy1H ∷ Alternative f ⇒ f a → f s → f (NonEmpty a)
sepBy1H parse sep = NonEmpty.fromList <$> sepBy1 parse sep

skipLiner ∷ Word8 → Parser ()
skipLiner p = spaceLiner (skip (== p))

maybeParend ∷ Parser a → Parser a
maybeParend p = p <|> parens p

--------------------------------------------------------------------------------
-- Expr Parser
--------------------------------------------------------------------------------

-- For Expressions
tableExp ∷ [[Expr.Operator ByteString Types.Expression]]
tableExp = [[infixOp]]

infixOp ∷ Expr.Operator ByteString Types.Expression
infixOp =
  Expr.Infix
    ( do
        inf ← spaceLiner infixSymbolDot
        pure
          (\l r → Types.Infix (Types.Inf l inf r))
    )
    Expr.AssocLeft

-- For Do!
table ∷ Semigroup a ⇒ [[Expr.Operator ByteString a]]
table = [[binary ";" (<>)]]

binary ∷ ByteString → (a → a → a) → Expr.Operator ByteString a
binary name fun = Expr.Infix (fun <$ spaceLiner (string name)) Expr.AssocLeft

--------------------------------------------------------------------------------
-- SN Derivatives
--------------------------------------------------------------------------------

topLevelSN ∷ Parser Types.TopLevel
topLevelSN = spaceLiner topLevel

expression'SN ∷ Parser Types.Expression
expression'SN = spaceLiner expression'

expressionSN ∷ Parser Types.Expression
expressionSN = spaceLiner expression

expressionS ∷ Parser Types.Expression
expressionS = spacer expression

signatureConstraintSN ∷ Parser [Types.TypeName]
signatureConstraintSN = spaceLiner signatureConstraint

argSN ∷ Parser Types.Arg
argSN = spaceLiner arg

matchLSN ∷ Parser Types.MatchL
matchLSN = spaceLiner matchL

matchLogicSN ∷ Parser Types.MatchLogic
matchLogicSN = spaceLiner matchLogic

matchLogicStartSN ∷ Parser Types.MatchLogicStart
matchLogicStartSN = spaceLiner matchLogicStart

matchLogicNamedSN ∷ Parser Types.MatchLogic
matchLogicNamedSN = spaceLiner matchLogicNamed

matchLogicNotNamedSN ∷ Parser Types.MatchLogic
matchLogicNotNamedSN = spaceLiner matchLogicNotNamed

nameSetSN ∷ Parser a → Parser (Types.NameSet a)
nameSetSN = spaceLiner . nameSet

moduleNameSN ∷ Parser Types.ModuleName
moduleNameSN = spaceLiner moduleName

condLogicSN ∷ Parser a → Parser (Types.CondLogic a)
condLogicSN = spaceLiner . condLogic

typePSN ∷ Parser Types.Type
typePSN = spaceLiner typeP

typePS ∷ Parser Types.Type
typePS = spacer typeP

typeSumParserSN ∷ Parser Types.TypeSum
typeSumParserSN = spaceLiner typeSumParser

typeSumParserS ∷ Parser Types.TypeSum
typeSumParserS = spacer typeSumParser

recordSN ∷ Parser Types.Record
recordSN = spaceLiner record

recordS ∷ Parser Types.Record
recordS = spacer record

nameTypeSN ∷ Parser Types.NameType
nameTypeSN = spaceLiner nameType

nameParserColonSN ∷ Parser Types.Name
nameParserColonSN = spaceLiner nameParserColon

nameParserSN ∷ Parser Types.Name
nameParserSN = spaceLiner nameParser

arrowTypeSN ∷ Parser Types.ArrowType
arrowTypeSN = spaceLiner arrowType

arrowTypeS ∷ Parser Types.ArrowType
arrowTypeS = spacer arrowType

bindingSN ∷ Parser Types.Binding
bindingSN = spaceLiner binding

typeRefineSN ∷ Parser Types.TypeRefine
typeRefineSN = spaceLiner typeRefine

typeRefineS ∷ Parser Types.TypeRefine
typeRefineS = spacer typeRefine

sumSN ∷ Parser Types.Sum
sumSN = spaceLiner sum

sumS ∷ Parser Types.Sum
sumS = spaceLiner sum

arrowsSN ∷ Parser Types.ArrowData
arrowsSN = spaceLiner arrows

parendArrowSN ∷ Parser Types.ArrowParen
parendArrowSN = spaceLiner parendArrow

typeNameParserSN ∷ Parser Types.TypeName
typeNameParserSN = spaceLiner typeNameParser

typeNameParserS ∷ Parser Types.TypeName
typeNameParserS = spacer typeNameParser

prefixCapitalDotSN ∷ Parser (NonEmpty Symbol)
prefixCapitalDotSN = spaceLiner prefixCapitalDot

prefixCapitalSN ∷ Parser Symbol
prefixCapitalSN = spaceLiner prefixCapital

prefixSymbolDotSN ∷ Parser (NonEmpty Symbol)
prefixSymbolDotSN = spaceLiner prefixSymbolDot

prefixSymbolSN ∷ Parser Symbol
prefixSymbolSN = spaceLiner prefixSymbol

prefixSymbolS ∷ Parser Symbol
prefixSymbolS = spacer prefixSymbol
