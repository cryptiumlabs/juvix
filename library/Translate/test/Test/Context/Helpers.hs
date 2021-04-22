module Test.Context.Helpers where

import qualified Juvix.Contextify as Contextify
import qualified Juvix.Contextify.ToContext.ResolveOpenInfo as Contextify
import qualified Juvix.Contextify.ToContext.Types as Contextify
import qualified Juvix.Core.Common.Context as Context
import qualified Juvix.Desugar as Desugar
import qualified Juvix.Frontend.Parser as Parser
import qualified Juvix.Frontend.Sexp as SexpTrans
import qualified Juvix.Frontend.Types.Base as Frontend
import Juvix.Library
import qualified Juvix.Library.Sexp as Sexp

----------------------------------------------------------------------
-- Give me sexp terms helpers
----------------------------------------------------------------------

contextualizeFoo ::
  ByteString ->
  IO
    ( Either
        Context.PathError
        (Contextify.ContextSexp, [Contextify.PreQualified])
    )
contextualizeFoo byte =
  Contextify.contextify (("Foo", parseDesugarSexp byte) :| [])

parseDesugarSexp :: ByteString -> [Sexp.T]
parseDesugarSexp = Desugar.op . parsedSexp

parsedSexp :: ByteString -> [Sexp.T]
parsedSexp xs = ignoreHeader (Parser.parse xs) >>| SexpTrans.transTopLevel

ignoreHeader :: Either a (Frontend.Header topLevel) -> [topLevel]
ignoreHeader (Right (Frontend.NoHeader xs)) = xs
ignoreHeader _ = panic "not no header"
