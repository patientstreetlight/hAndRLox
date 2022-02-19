module Env (Env, define, get, set, newRootEnv, newNestedEnv, parent) where

import Data.Map
import qualified Data.Map as M

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

get :: (Ord k, Show k) => k -> Env k v -> v
get var = go . Just
  where
    go env = case env of
        Nothing -> error $ "undefined variable " ++ show var
        Just e -> case M.lookup var $ vals e of
            Nothing -> go $ parent e
            Just val -> val

set :: (Ord k, Show k) => k -> v -> Env k v -> Env k v
set var val = go . Just
  where
    go env = case env of
        Nothing -> error $ "undefined variable " ++ show var
        Just e ->
            if M.member var (vals e) then
                define var val e
            else
                e { parent = Just $ go $ parent e }