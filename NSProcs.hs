module NSProcs (nsProcs) where

import Common
import Core (evalTcl)
import qualified TclObj as T

nsProcs = makeProcMap [("namespace", procNamespace), ("variable", procVariable)]

procNamespace :: [T.TclObj] -> TclM RetVal
procNamespace = makeEnsemble "namespace" [
     ("current", ns_current),
     ("eval", ns_eval),
     ("parent", ns_parent),
     ("children", ns_children),
     ("delete", ns_delete),
     ("exists", ns_exists)]

procVariable args = case args of
       [n,v] -> varSet (T.asBStr n) v
       _     -> argErr "variable"

ns_current args = case args of
       [] -> currentNS >>= treturn
       _  -> argErr "namespace current"

ns_eval args = case args of
          [nsname, code] -> withNS (T.asBStr nsname) (evalTcl code)
          _              -> argErr "namespace eval"

ns_parent args = case args of
          [] -> parentNS >>= treturn
          _  -> argErr "namespace parent"

ns_children args = case args of
          [] -> childrenNS >>= return . T.mkTclList . map T.mkTclBStr
          _  -> argErr "namespace children"

ns_exists args = case args of
          [nsn] -> existsNS (T.asBStr nsn) >>= return . T.fromBool
          _     -> argErr "namespace exists"

ns_delete args = case args of
   [nsn] -> deleteNS (T.asBStr nsn) >> ret
   _     -> argErr "namespace delete"
