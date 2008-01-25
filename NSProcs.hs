module NSProcs (nsProcs) where
import Util
import Common
import qualified TclObj as T

nsProcs = makeProcMap [("namespace", procNamespace)]

procNamespace = makeEnsemble "namespace" [("current", namespace_current), ("eval", namespace_eval)]

namespace_current args = case args of 
       [] -> treturn (pack "::")
       _  -> argErr "namespace current"

namespace_eval args = case args of
          [nsname, code] -> tclErr "not implemented"
          _              -> argErr "namespace eval"
                     
