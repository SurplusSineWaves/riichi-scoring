{- |
Module      : Riichi.Display
Description : Functions handling UI. Pending major revision.
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Display where

import ColourStrings
import Control.Monad (forM, forM_, when)
import Control.Monad.Trans
import Data.Function
import Data.List (intercalate, intersperse, sort)
import Data.Monoid (getSum)
import Riichi.Context
import Riichi.Efficiency
import Riichi.Meld
import Riichi.Scoring
import Riichi.Shanten
import Riichi.Tile
import Riichi.Waits
import Riichi.Yaku

-- | Implements the "yaku" command for the CLI.
displayHandYaku :: Hand -> IO ()
displayHandYaku hand = do
    if length hand < 14
        then
            putStrLn "Hand is the wrong size"
        else do
            let ihs = interpretHand hand
            putStrLn "🀀 🀁 🀂 🀃 "
            putStrLn "Assuming East round and East seat."
            let num = length ihs
            when (num >= 1) $ do
                if num == 1
                    then putStrLn "Found 1 way to interpret this hand:\n"
                    else putStrLn $ "Found " ++ show num ++ " ways to interpret this hand:\n"
                forM_ ihs $ \ih -> do
                    let handString = showInterpretedHand ih
                    let handContext = getMinimalHandContext hand False
                    maybeYakumanContext <- mkYakumanContext hand (Just ih) (Just True)
                    let context = case maybeYakumanContext of
                            Nothing ->
                                let yakuContext = mkYakuContext hand (Just ih) handContext
                                 in Context (Just ih) handContext (Left yakuContext)
                            Just yakumanContext -> Context (Just ih) handContext (Right yakumanContext)
                    let string = formContextString context
                    let hanOrYakumans = getContextHansOrYakumans context
                    case hanOrYakumans of
                        Left (hanClosed, hanOpen) ->
                            putStrLn $
                                handString
                                    ++ "\n"
                                    ++ string
                                    ++ "\t\t"
                                    ++ toGreen (show (getSum hanClosed))
                                    ++ " Han total if closed, "
                                    ++ toGreen (show (getSum hanOpen))
                                    ++ " if open\n"
                        Right yakumans -> putStrLn $ handString ++ "\n" ++ string ++ "\t\t" ++ toGreen (show yakumans) ++ " Yakuman total\n"
            if chiitoitsu hand || thirteenOrphans hand
                then do
                    if num == 0
                        then putStrLn "This hand can be interpreted as:\n"
                        else putStrLn "This hand can also be interpreted as:\n"
                    let handString = intercalate ", " (hand & sort & map show)
                    let handContext = getMinimalHandContext hand True
                    maybeYakumanContext <- mkYakumanContext hand Nothing (Just True)
                    let context = case maybeYakumanContext of
                            Nothing ->
                                let yakuContext = mkYakuContext hand Nothing handContext
                                 in Context Nothing handContext (Left yakuContext)
                            Just yakumanContext -> Context Nothing handContext (Right yakumanContext)
                    let string = formContextString context
                    let hanOrYakumans = getContextHanOrYakumans context
                    putStrLn $ case hanOrYakumans of
                        Left han -> handString ++ "\n" ++ string ++ "\t\t" ++ toGreen (show (getSum han)) ++ " Han total, closed by definition\n"
                        Right yakumans -> handString ++ "\n" ++ string ++ "\t\t" ++ toGreen (show yakumans) ++ " Yakuman total\n"
                else
                    Control.Monad.when (num == 0) $ putStrLn $ toRed "This hand is not valid"

-- | Implements the "waits" command for the CLI.
displayHandWaits :: Hand -> IO ()
displayHandWaits hand = do
    let waits = getWaits hand
    putStrLn $ "Waits are: " ++ intercalate ", " (map show waits)

-- | Implements the "score" command for the CLI.
displayHandScore :: Hand -> IO ()
displayHandScore hand = do
    if length hand < 14
        then putStrLn "Hand is the wrong size"
        else do
            context@(Context _ handContext _) <- mkContext hand
            let string = formContextString context
            let tsumo = isTsumo handContext
            let closure = isClosed handContext
            let hanOrYakumans = getContextHanOrYakumans context
            case hanOrYakumans of
                Left han ->
                    if getContextDora context == getSum han
                        then
                            putStrLn $ "\n\t" ++ toRed "Hand has no yaku!"
                        else do
                            let fu = getContextFu context
                            let name = hanFuToHandName han fu
                            let openClosed = if closure then "Closed hand: " else "Open hand: "
                            putStrLn $
                                "\tYaku:\n"
                                    ++ string
                                    ++ "\t\t"
                                    ++ openClosed
                                    ++ toGreen (show (getSum han))
                                    ++ " Han total, with "
                                    ++ toBlue (show fu)
                                    ++ " Fu\n"
                                    ++ "\n\t"
                                    ++ toGreen (show (getScore han fu True tsumo))
                                    ++ " points for Dealer, "
                                    ++ toGreen (show (getScore han fu False tsumo))
                                    ++ " points for Non-Dealer"
                                    ++ ( if name /= ""
                                            then
                                                " ("
                                                    ++ toMagenta name
                                                    ++ ")."
                                            else ""
                                       )
                Right yakumans ->
                    putStrLn $
                        string
                            ++ "\t\t"
                            ++ toGreen (show yakumans)
                            ++ " Yakuman total\n"
                            ++ "\n\t"
                            ++ toGreen (show (yakumans * 48000))
                            ++ " points for Dealer, "
                            ++ toGreen (show (yakumans * 32000))
                            ++ " points for Non-Dealer."

displayHandShanten :: Hand -> IO ()
displayHandShanten hand = do
    let shanten = getShanten hand
    case shanten of
        -1 -> putStrLn $ "Hand is: " ++ toMagenta (show shanten ++ " shanten") ++ toBlue " (Complete)"
        0 -> putStrLn $ "Hand is: " ++ toMagenta (show shanten ++ " shanten") ++ toBlue " (Tenpai)"
        1 -> putStrLn $ "Hand is: " ++ toMagenta (show shanten ++ " shanten") ++ toBlue " (Iishanten)"
        _ -> putStrLn $ "Hand is: " ++ toMagenta (show shanten ++ " shanten")

displayHandDiscard :: Hand -> IO ()
displayHandDiscard hand = do
    let discards = optimalDiscards hand
    putStrLn $ "Optimal discards are: " ++ intercalate ", " (map show discards)
