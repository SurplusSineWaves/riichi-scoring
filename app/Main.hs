{- |
Module      : Main
Description : Contains entrypoint for CLI in main function.
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Main (main) where

import Riichi.Display
import Riichi.Meld
import System.Environment (getArgs)

-- | Entrypoint for CLI executable riichi.
main :: IO ()
main = do
    args <- getArgs
    let list = ['1' .. '9'] ++ "NESWrgw"
    case args of
        [] -> putStrLn "No arguments supplied. Run riichi --help for more info."
        (arg : [])
            | (arg `elem` ["--help", "-h", "help"]) -> putStrLn helpString
            | (arg `elem` ["yaku", "waits", "score"]) -> putStrLn "Missing hand"
            | (head arg) `elem` list -> displayHandYaku $ mkHand arg
            | otherwise -> putStrLn "Command not recognised"
        (arg1 : arg2 : _)
            | arg1 == "yaku" -> displayHandYaku $ mkHand arg2
            | arg1 == "waits" -> displayHandWaits $ mkHand arg2
            | arg1 == "score" -> displayHandScore $ mkHand arg2
            | otherwise -> putStrLn "Command not recognised"

helpString :: String
helpString =
    "Command riichi:\n\tPossible subcommands: yaku, waits, score (default = yaku)\n\nUsage:\
    \\n\triichi <subcommand> \"<hand>\"\
    \\n\n\t\"yaku\" and \"score\" subcommands expect a full hand.\
    \\n\t\"waits\" subcommand expects a hand that is tenpai.\
    \\n\n\tExample hands include:\
    \\n\t\t\"123p 234m 444p rrrr NN\"\
    \\n\t\t\"344556s 444p 222m EE\"\
    \\n\t\t\"19p 19s 19m 1p NESWrgw\"\
    \\n\n\tIn detail, numeric tiles are denoted (1-9) + (m, p, or s),\
    \\n\tWinds are denoted N, E, S, W, and Dragons are r, w, g.\
    \\n\tA 0 can be used to denote a red five.\
    \\n\tNumeric tiles of the same suit, and honour tiles, can be \
    \\n\tgrouped as seen in the examples (but needn't be).\
    \\n\n\tIn scoring a hand, dora and seat/round wind must be supplied,\
    \\n\talso in this format."
