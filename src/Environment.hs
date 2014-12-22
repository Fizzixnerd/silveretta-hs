module Environment(Env(Env),
                   binds, up,
                   empty,
                   addBind,
                   pushEnv,
                  lookup)
       where

import qualified Data.Map as Map
import qualified Forms as F
import Prelude hiding (lookup)

data Env = Env { binds :: (Map.Map F.BindingVar F.Form), up :: (Maybe Env) }

empty :: Env
empty = Env { binds = Map.empty, up = Nothing }

addBind :: F.Binding -> Env -> Env
addBind (F.Binding {F.var = var, F.val = val}) (Env {binds = binds, up = up}) =
  Env {binds = Map.insert var val binds, up = up}

-- | Pushes Env new onto Env old, returning Env new with `up' set to Env old.
pushEnv :: Env -> Env -> Env
pushEnv old (Env {binds = binds, up = Nothing}) = Env {binds = binds, up = Just old}

lookup :: F.BindingVar -> Env -> Maybe F.Form
lookup b (Env {binds = binds, up = Nothing}) = Map.lookup b binds
lookup b (Env {binds = binds, up = Just up}) =
  case (Map.lookup b binds) of
    Nothing -> lookup b up
    Just f -> Just f
