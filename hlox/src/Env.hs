module Env (Env, define, resolve, newRootEnv, newNestedEnv, parent) where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Applicative

data Env k v = Env
    { parent :: Maybe (Env k v)
    , vals :: Map k v
    } deriving (Show)

newRootEnv :: Env k v
newRootEnv = Env
    { parent = Nothing
    , vals = M.empty
    }

newNestedEnv :: Env k v -> Env k v
newNestedEnv p = Env
    { parent = Just p
    , vals = M.empty
    }

define :: Ord k => k -> v -> Env k v -> Env k v
define var val env =
    env { vals = M.insert var val $ vals env }

resolve :: Ord k => k -> Env k v -> Maybe v
resolve var = go
  where
    go env = M.lookup var (vals env) <|> (parent env >>= go)