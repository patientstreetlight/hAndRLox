module Env (Env, define, get, set, newRootEnv, newNestedEnv, parent) where

import Data.Map
import qualified Data.Map as M
import Data.Text
import Value (Value)

data Env = Env
    { parent :: Maybe Env
    , vals :: Map Text Value
    } deriving (Show)

newRootEnv :: Env
newRootEnv = Env
    { parent = Nothing
    , vals = M.empty
    }

newNestedEnv :: Env -> Env
newNestedEnv p = Env
    { parent = Just p
    , vals = M.empty
    }

define :: Text -> Value -> Env -> Env
define var val env =
    env { vals = M.insert var val $ vals env }

get :: Text -> Env -> Value
get var = go . Just
  where
    go :: Maybe Env -> Value
    go env = case env of
        Nothing -> error $ "undefined variable " ++ unpack var
        Just e -> case M.lookup var $ vals e of
            Nothing -> go $ parent e
            Just val -> val


set :: Text -> Value -> Env -> Env
set var val = go . Just
  where
    go :: Maybe Env -> Env
    go env = case env of
        Nothing -> error $ "undefined variable " ++ unpack var
        Just e ->
            if M.member var (vals e) then
                define var val e
            else
                e { parent = Just $ go $ parent e }