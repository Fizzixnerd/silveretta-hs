module Global(globalEnv)
       where

import qualified Data.Map as Map
import qualified Environment as E
import qualified Forms as F
import qualified Intrinsics as I

globalEnv :: E.Env
globalEnv = E.Env {
  E.binds = Map.fromList [("+", F.Function I.addl),
                          ("-", F.Function I.subl),
                          ("*", F.Function I.mull),
                          ("/", F.Function I.divl)],
  E.up = Nothing
  }
