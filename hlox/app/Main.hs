module Main where

import System.Environment (getArgs)
import System.Exit (die)
import System.Console.Haskeline
import Lexer (lexLox)
import qualified Data.Text as T
import Token (dtToken)
import Parser
import Eval

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runPrompt
        [fileName] -> runFile fileName
        _ -> die "Usage: hlox [script]"


runPrompt :: IO ()
runPrompt = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "> "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just "" -> loop
            Just input -> do
                outputStrLn $ evalLine input
                loop

evalLine :: String -> String
evalLine s = case lexLox (T.pack s) of
    Left err -> error "error lexing"
    Right tokens -> show $ eval $ parseLox $ map dtToken tokens


runFile :: String -> IO ()
runFile fileName = do
    putStrLn "running file "