{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main (main) where

import Display
import Meld
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

-- if (head (head args) `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9', 'N', 'E', 'W', 'S', 'r', 'g', 'w'])
--     then do
--         let hand = mkHand $ head $ args
--         displayHandYaku hand
--     else
--         if head args == "yaku"
--             then do
--                 let hand = mkHand $ head $ tail $ args
--                 displayHandYaku hand
--             else putStrLn ("Command not recognised.")
