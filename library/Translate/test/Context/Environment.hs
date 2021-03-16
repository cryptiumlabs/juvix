module Context.Environment (top) where

import qualified Data.HashSet as Set
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types.Base as Frontend
import qualified Juvix.FrontendContextualise as Contextualize
import qualified Juvix.FrontendContextualise.Environment as Env
import Juvix.Library
import qualified Juvix.Library.HashMap as Map
import qualified Juvix.Library.Sexp as Sexp
import qualified Test.Tasty as T
import qualified Test.Tasty.HUnit as T
import Prelude (error, fail)

--------------------------------------------------------------------------------
-- Top Level Test
--------------------------------------------------------------------------------

top :: T.TestTree
top =
  T.testGroup
    "testing environment functions"
    [passContext]

--------------------------------------------------------------------------------
-- Environment Runner Types
--------------------------------------------------------------------------------
data Capture
  = Cap
      { closure :: Env.Closure',
        report :: [Env.Closure']
      }
  deriving (Generic, Show)

type CaptureAlias =
  ExceptT Env.ErrorS (State Capture)

newtype Context a = Ctx {run :: CaptureAlias a}
  deriving (Functor, Applicative, Monad)
  deriving
    ( HasReader "closure" Env.Closure',
      HasSource "closure" Env.Closure'
    )
    via ReaderField "closure" CaptureAlias
  deriving
    ( HasWriter "report" [Env.Closure'],
      HasSink "report" [Env.Closure']
    )
    via WriterField "report" CaptureAlias
  deriving
    (HasThrow "error" Env.ErrorS)
    via MonadError CaptureAlias

runCtx :: Context a -> Capture -> (Either Env.ErrorS a, Capture)
runCtx (Ctx c) = runState (runExceptT c)

emptyCtx :: IO (Context.T Sexp.T Sexp.T Sexp.T)
emptyCtx = Context.empty "Foo"

emptyClosure :: Capture
emptyClosure = Cap (Env.Closure Map.empty) []

recordClosure ::
  (HasReader "closure" a m, HasWriter "report" [a] m) => p -> b -> m b
recordClosure _atom t = do
  c <- ask @"closure"
  tell @"report" [c]
  -- Just drop the given atom
  pure t

--------------------------------------------------------------------------------
-- PassContext Tests
--------------------------------------------------------------------------------

passContext :: T.TestTree
passContext =
  T.testGroup
    "testing passContext closures"
    [letTest]

----------------------------------------
-- Let Test
----------------------------------------

letTest :: T.TestTree
letTest =
  T.testGroup
    "Let's properly add to the closure"
    [ T.testCase "let-match" $ do
        Right (ctx, _) <-
          contextualizeFoo
            "let f = let g = 3 in \
            \ let foo (Nil x)      = print-closure 0 in \
            \ let foo (Cons a y) z = print-closure 0 in \
            \ 3"
        let (_, Cap _ capture) =
              runCtx (Env.passContextSingle ctx trigger recordClosure) emptyClosure
        case capture of
          [Env.Closure x, Env.Closure y] -> do
            Map.keysSet x T.@=? firstClosure
            Map.keysSet y T.@=? secondClosure
          _ ->
            fail "not enough captured closures"
    ]
  where
    firstClosure =
      Set.fromList ["g", "foo", "x"]
    secondClosure =
      Set.fromList ["g", "foo", "a", "y", "z"]
    trigger =
      (== "print-closure")

-- Right (ctx,_) <- contextualizeFoo "let f = 3"
-- (_, capture) = runCtx (Env.passContextSingle ctx (== "print-closure") recordClosure) emptyClosure
----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

contextualizeFoo byte =
  Contextualize.contextifyS (("Foo", parseDesugarSexp byte) :| [])

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"

ignoreRight :: Either a p -> p
ignoreRight (Right x) = x
ignoreRight (Left _) = error "not right"
