module NSProcs (nsProcs) where
import Util
import Common
import qualified TclObj as T

nsProcs = makeProcMap [("namespace", procNamespace)]

procNamespace = makeEnsemble "namespace" [("current", ns_current), ("eval", ns_eval), ("parent", ns_parent)]

ns_current args = case args of 
       [] -> treturn (pack "::")
       _  -> argErr "namespace current"

ns_eval args = case args of
          [nsname, code] -> tclErr "not implemented"
          _              -> argErr "namespace eval"
                     
ns_parent args = case args of
          [] -> ret
          _  -> argErr "namespace parent"
