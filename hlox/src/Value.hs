module Value where

import Data.Text
import qualified Data.Text as T

data Value
    = Number Double
    | Bool Bool
    | String Text
    | Nil
    deriving Eq


instance Show Value where
    show v = case v of
        Number n -> show n
        Bool True -> "true"
        Bool False -> "false"
        String s -> T.unpack s
        Nil -> "nil"