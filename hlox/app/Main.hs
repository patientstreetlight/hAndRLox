module Main where

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runPrompt
        [fileName] -> runFile fileName
        _ -> die "Usage: hlox [script]"


runPrompt :: IO ()
runPrompt = do
    putStr "> "
    -- XXX more here


runFile :: String -> IO ()
runFile fileName = do
    putStrLn "running file "