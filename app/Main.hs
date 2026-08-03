{- |
Module      : Main
Description : Contains entrypoint for CLI in main function.
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Main (main) where

import Data.Version (showVersion)
import Paths_riichi_scoring (version)
import Riichi.Display
import Riichi.Display (displayLiveMode)
import Riichi.Meld
import System.Environment (getArgs)

-- | Entrypoint for CLI executable riichi.
main :: IO ()
main = do
    args <- getArgs
    let list = ['1' .. '9'] ++ "NESWrgw"
    case args of
        [] -> putStrLn "No arguments supplied. Run riichi --help for more info."
        [arg]
            | arg `elem` ["--help", "-h", "help"] -> putStrLn helpString
            | arg `elem` ["--version", "-v", "version"] -> putStrLn $ "riichi-scoring: " ++ showVersion version
            | arg `elem` ["yaku", "waits", "score"] -> putStrLn "Missing hand"
            | head arg `elem` list -> displayHandYaku $ mkHand arg
            | otherwise -> putStrLn "Command not recognised"
        (arg1 : arg2 : _)
            | arg1 == "yaku" -> displayHandYaku $ mkHand arg2
            | arg1 == "waits" -> displayHandWaits $ mkHand arg2
            | arg1 == "score" -> displayHandScore $ mkHand arg2
            | arg1 == "shanten" -> displayHandShanten $ mkHand arg2
            | arg1 == "discard" -> displayHandDiscard $ mkHand arg2
            | arg1 == "live" -> displayLiveMode $ mkHand arg2
            | otherwise -> putStrLn "Command not recognised"

helpString :: String
helpString =
    "Command riichi: \n\tversion = "
        ++ showVersion version
        ++ "\nUsage:\
           \\n\triichi <subcommand> \"<hand>\"\n\
           \\n\tSubcommands:\
           \\n\t\tyaku      Determine yaku of a completed hand.\
           \\n\t\twaits     Determine the waits of a ready hand.\
           \\n\t\tscore     Score a completed hand.\
           \\n\t\tshanten   Get the shanten of a hand.\
           \\n\t\tdiscard   Determine which discard for a hand has the best \
           \\n\t\t          tile efficiency.\
           \\n\t\tlive      Interactive mode. Give discard recommendations for \
           \\n\t\t          a hand until it is complete.\
           \\n\n\t\"yaku\", \"score\", \"discard\" and \"live\" subcommands expect a full hand.\
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
           \\n\n\tIn scoring a hand, dora and seat/round wind may be required,\
           \\n\talso supplied in this format.\n"
