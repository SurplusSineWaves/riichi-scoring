module Main (main) where

import Riichi.Display
import Riichi.Meld
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let list = ['1' .. '9'] ++ "NESWrgw"
    case args of
        [] -> putStrLn "No arguments supplied"
        (arg : [])
            | (arg `elem` ["--help", "-h", "help"]) -> putStrLn "Command riichi:\n\tPossible subcommands: yaku, waits, score (default = yaku)\n\nUsage: mahjong <subcommand> \"<hand>\""
            | (arg `elem` ["yaku", "waits", "score"]) -> putStrLn "Missing hand"
            | (head arg) `elem` list -> displayHandYaku $ mkHand arg
            | otherwise -> putStrLn "Command not recognised"
        (arg1 : arg2 : _)
            | arg1 == "yaku" -> displayHandYaku $ mkHand arg2
            | arg1 == "waits" -> displayHandWaits $ mkHand arg2
            | arg1 == "score" -> displayHandScore $ mkHand arg2
            | otherwise -> putStrLn "Command not recognised"
