{-# LANGUAGE BangPatterns,OverloadedStrings #-}
module TclLib.NSCmds (nsCmds) where

import Common
import Core ()
import TclLib.LibUtil
import VarName
import qualified TclObj as T

nsCmds = makeCmdList [("namespace", cmdNamespace), ("variable", cmdVariable)]

cmdNamespace = mkEnsemble "namespace" [
     ("current", ns_current),
     ("eval", ns_eval),
     ("parent", ns_parent),
     ("children", ns_children),
     ("delete", ns_delete),
     ("tail", ns_tail),
     ("path", ns_path),
     ("export", ns_export),
     ("import", ns_import),
     ("unknown", ns_unknown),
     ("forget", ns_forget),
     ("origin", ns_origin),
     ("qualifiers", ns_qualifiers),
     ("exists", ns_exists)]

cmdVariable args = case args of
       []    -> vArgErr "variable ?name value...? name ?value?"
       _     -> go args
  where go (n:v:xs) = variableNS (T.asBStr n) (Just v) >> go xs
        go [n]      = variableNS (T.asBStr n) Nothing >> ret
        go []       = ret

ns_current args = case args of
       [] -> currentNS >>= treturn
       _  -> vArgErr "namespace current"

ns_eval args = case args of
          [nsname, code] -> withNS (T.asBStr nsname) (evalTcl code)
          _              -> argErr "namespace eval"

ns_parent args = case args of
          [] -> parentNS Nothing >>= treturn
          [ns] -> parentNS (Just (parseNSTag (T.asBStr ns))) >>= treturn
          _  -> vArgErr "namespace parent ?namespace?"

ns_export args = case map T.asBStr args of
       []     -> getExportsNS >>= return . T.fromList . map T.fromBStr
       ("-clear":al) -> mapM_ (exportNS True) al >> ret
       al            -> mapM_ (exportNS False) al >> ret
          
ns_import args = case map T.asBStr args of
      ("-force":rest) -> mapM_ (importNS True) rest >> ret
      al              -> mapM_ (importNS False) al >> ret

ns_forget args = mapM_ (forgetNS . T.asBStr) args >> ret

ns_origin args = case args of
     [pn] -> do pr <- getCmd (T.asBStr pn)
                case pr of
                  Nothing -> tclErr $ "invalid command name: " ++ show pn
                  Just p  -> getOriginName p >>= treturn
     _    -> argErr "namespace origin"

ns_children args = case args of
          [] -> childrenNS Nothing >>= return . T.fromList . map T.fromBStr
          [nsn] -> childrenNS (Just (parseNSTag (T.asBStr nsn))) >>= return . T.fromList . map T.fromBStr
          _  -> argErr "namespace children"

ns_exists args = case args of
          [nsn] -> existsNS (T.asBStr nsn) >>= return . T.fromBool
          _     -> argErr "namespace exists"

ns_delete args = mapM_ (deleteNS . T.asBStr) args >> ret

ns_tail args = case args of
   [s] -> treturn . nsTail . parseNSTag . T.asBStr $ s
   _   -> vArgErr "namespace tail string"

ns_path args = case args of
   [] -> getPathNS >>= return . T.fromList . map T.fromBStr
   [lst] -> T.asList lst >>= mapM_ (\n -> addToPathNS (parseNSTag (T.asBStr n))) >> ret
   _ -> vArgErr "namespace path ?pathList?"

ns_qualifiers args = case args of
   [s] -> return . T.fromBStr $ (nsQualifiers (T.asBStr s))
   _   -> vArgErr "namespace qualifiers string"

ns_unknown args = case args of
   [] -> getUnknownNS >>= maybe ret treturn
   [n] -> setUnknownNS (T.asBStr n) >> ret
   _   -> vArgErr "namespace unknown ?script?"
   
