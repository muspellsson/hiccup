module NSProcs (nsProcs) where
import Util
import Common
import Core (evalTcl)
import qualified TclObj as T

nsProcs = makeProcMap [("namespace", procNamespace)]

procNamespace = makeEnsemble "namespace" [("current", ns_current), ("eval", ns_eval), ("parent", ns_parent)]

ns_current args = case args of 
       [] -> currentNS >>= treturn
       _  -> argErr "namespace current"

ns_eval args = case args of
          [nsname, code] -> withNS (T.asBStr nsname) (evalTcl code)
          _              -> argErr "namespace eval"
                     
ns_parent args = case args of
          [] -> parentNS >>= treturn
          _  -> argErr "namespace parent"
