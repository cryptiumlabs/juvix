module Test.Sexp.Helpers where

import qualified Juvix.Contextify as Contextify
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types.Base as Frontend
import Juvix.Library
import qualified Juvix.Library.NameSymbol as NameSymbol
import qualified Juvix.Library.Sexp as Sexp
import Prelude (error)

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

unwrapLookup :: NameSymbol.T -> Context.T a ty sumRep -> Maybe a
unwrapLookup symbol ctx =
  case Context.lookup symbol ctx >>| Context.extractValue of
    Just ((Context.Def Context.D {defTerm})) ->
      Just defTerm
    _ -> Nothing

contextualizeFoo ::
  ByteString -> IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextualizeFoo byte =
  Contextify.op
    ( ( "A",
        parseDesugarSexp
          "declare infixl (+) 8 \
          \ let (+) = 3 \
          \ declare infixl (*) 9 \
          \ let (*) = 3 \
          \ declare infix (**) 7 \
          \ let (**) = 3 \
          \ declare infix (***) 7 \
          \ let (**) = 3 \
          \ let a = 3 \
          \ let x = 2 "
      )
        :| [("Foo", parseDesugarSexp byte)]
    )

contextualizeFooAmbi ::
  ByteString -> IO (Either Contextify.ResolveErr (Context.T Sexp.T Sexp.T Sexp.T))
contextualizeFooAmbi byte =
  Contextify.op
    ( ( "A",
        parseDesugarSexp "declare infixl (+) 9 let (+) = 3 "
      )
        :| [ ("B", parseDesugarSexp "declare infixl (+) 9 let (+) = 3"),
             ("Foo", parseDesugarSexp byte)
           ]
    )

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = error "not no header"
