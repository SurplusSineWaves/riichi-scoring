{- |
Module      : Riichi.Display
Description : Functions handling UI. Pending major revision.
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Display where

import ColourStrings
import Control.Monad (forM)
import Control.Monad.Trans
import Data.Function
import Data.List (intersperse, sort)
import Data.Monoid (getSum)
import Riichi.Context
import Riichi.Meld
import Riichi.Scoring
import Riichi.Tile
import Riichi.Waits
import Riichi.Yaku

-- | Implements the "yaku" command for the CLI. In need of a refactor.
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
            if num >= 1
                then do
                    if num == 1
                        then putStrLn $ "Found 1 way to interpret this hand:\n"
                        else putStrLn $ "Found " ++ show num ++ " ways to interpret this hand:\n"
                    _ <- forM ihs $ \ih -> do
                        let handString = showInterpretedHand ih
                        let handContext = getMinimalHandContext hand False
                        maybeYakumanContext <- mkYakumanContext hand (Just ih) (Just True)
                        let context = case maybeYakumanContext of
                                Nothing ->
                                    let yakuContext = mkYakuContext hand (Just ih) handContext
                                     in (Context (Just ih) handContext (Left yakuContext))
                                Just yakumanContext -> (Context (Just ih) handContext (Right yakumanContext))
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
                    return ()
                else return ()
            if (chiitoitsu hand) || (thirteenOrphans hand)
                then do
                    if num == 0
                        then putStrLn "This hand can be interpreted as:\n"
                        else putStrLn "This hand can also be interpreted as:\n"
                    let handString = hand & sort & map show & intersperse ", " & concat
                    let handContext = getMinimalHandContext hand True
                    maybeYakumanContext <- mkYakumanContext hand Nothing (Just True)
                    let context = case maybeYakumanContext of
                            Nothing ->
                                let yakuContext = mkYakuContext hand Nothing handContext
                                 in (Context Nothing handContext (Left yakuContext))
                            Just yakumanContext -> (Context Nothing handContext (Right yakumanContext))
                    let string = formContextString context
                    let hanOrYakumans = getContextHanOrYakumans context
                    putStrLn $ case hanOrYakumans of
                        Left han -> handString ++ "\n" ++ string ++ "\t\t" ++ toGreen (show (getSum han)) ++ " Han total, closed by definition\n"
                        Right yakumans -> handString ++ "\n" ++ string ++ "\t\t" ++ toGreen (show yakumans) ++ " Yakuman total\n"
                else
                    if num == 0
                        then putStrLn $ toRed "This hand is not valid"
                        else return ()

-- | Implements the "waits" command for the CLI.
displayHandWaits :: Hand -> IO ()
displayHandWaits hand = do
    let waits = getWaits hand
    putStrLn $ "Waits are: " ++ (waits & map show & intersperse ", " & concat)

-- | Implements the "score" command for the CLI. In need of a refactor, logic is rather serpentine at the moment.
displayHandScore :: Hand -> IO ()
displayHandScore hand = do
    putStrLn "Input dora (or leave blank):"
    dora <- mkHand <$> getLine
    if dora /= []
        then
            putStrLn ""
        else return ()

    let hand' = addDora dora hand
    context@(Context _ handContext _) <- mkContext hand'
    let string = formContextString context
    let tsumo = isTsumo handContext
    let closure = isClosed handContext
    let hanOrYakumans = getContextHanOrYakumans context
    case hanOrYakumans of
        Left han -> do
            let fu = getContextFu context
            let name = hanToHandName han
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
          where
            name = hanToHandName han
            openClosed = if closure then "Closed hand: " else "Open hand: "
        Right yakumans ->
            putStrLn $
                string
                    ++ "\t\t"
                    ++ toGreen (show (yakumans))
                    ++ " Yakuman total\n"
                    ++ "\n\t"
                    ++ toGreen (show (yakumans * 48000))
                    ++ " points for Dealer, "
                    ++ toGreen (show (yakumans * 32000))
                    ++ " points for Non-Dealer."

-- putStrLn "Input round and seat wind: "
-- (Honour (Wind roundWind) _) : (Honour (Wind seatWind) _) : _ <- mkHand <$> getLine
-- putStrLn "Riichi? [y/n]: "
-- riichi <- (== "y") <$> getLine
-- ippatsu <-
--     if riichi
--         then do
--             putStrLn "Ippatsu? [y/n]: "
--             (== "y") <$> getLine
--         else return False
-- putStrLn "Tsumo? [y/n]: "
-- input <- getLine
-- let tsumo = (input == "y")
--
-- sevenPairs <-
--     if chiitoitsu hand'
--         then do
--             putStrLn "Seven pairs? [y/n]: "
--             input <- getLine
--             if input == "y"
--                 then
--                     return True
--                 else
--                     return False
--         else return False

-- if sevenPairs == True
--     then do
--         let (value, yaku_string) = getYaku hand' Nothing riichi ippatsu tsumo False seatWind roundWind True
--         putStrLn $ case value of
--             Left (han, _) ->
--                 yaku_string
--                     ++ "\t\t"
--                     ++ toGreen (show (getSum han))
--                     ++ " Han total, closed by definition\n"
--                     ++ "\n\t"
--                     ++ toGreen (show (getScore han 25 True tsumo))
--                     ++ " points for Dealer, "
--                     ++ toGreen (show (getScore han 25 False tsumo))
--                     ++ " points for Non-Dealer."
--                     ++ ( if name /= ""
--                             then
--                                 " ("
--                                     ++ toMagenta name
--                                     ++ ")."
--                             else ""
--                        )
--               where
--                 name = hanToHandName han
--             Right yakumans ->
--                 yaku_string
--                     ++ "\t\t"
--                     ++ toGreen (show (getSum yakumans))
--                     ++ " Yakuman total\n"
--                     ++ "\n\t"
--                     ++ toGreen (show (getSum yakumans * 48000))
--                     ++ " points for Dealer, "
--                     ++ toGreen (show (getSum yakumans * 32000))
--                     ++ " points for Non-Dealer."
--     else do
--         let ihs = interpretHand hand'
--         maybeIh <-
--             if thirteenOrphans hand'
--                 then return Nothing
--                 else do
--                     (pair, melds) <-
--                         if length ihs > 1
--                             then do
--                                 putStrLn "Select hand interpretation: "
--                                 sequence_ $ [("[" ++ show n ++ "]: " ++ (ih & showInterpretedHand)) & putStrLn | (n :: Integer, ih) <- zip [0 ..] ihs]
--                                 n <- read <$> getLine :: IO Int
--                                 return (ihs !! n)
--                             else do
--                                 putStrLn "Found one way to interpret this hand: "
--                                 let ih = head ihs
--                                 putStrLn (showInterpretedHand ih)
--                                 return ih
--
--                     melds' <- case (riichi, tsumo) of
--                         (True, True) -> return melds
--                         (True, False) -> getRonMeld melds
--                         -- putStrLn "Which meld was opened by Ron? (enter an index): "
--                         -- sequence_ $ [("[" ++ show i ++ "]: " ++ (meld & show)) & putStrLn | (i :: Integer, meld) <- zip [0 ..] melds]
--                         -- input <- getLine
--                         -- let index :: Int = input & read
--                         -- return $ (zip [0 ..] melds) & map (\(i, meld) -> if i == index then openMeld meld else meld
--                         (False, _) -> getOpenMelds melds
--                     -- putStrLn "Which melds are open? (enter a string of indices, or leave blank if all closed): "
--                     -- sequence_ $ [("[" ++ show i ++ "]: " ++ (meld & show)) & putStrLn | (i :: Integer, meld) <- zip [0 ..] melds]
--                     -- input <- getLine
--                     -- let indices :: [Int] = input & map return & (map read)
--                     -- return $ (zip [0 ..] melds) & map (\(i, meld) -> if i `elem` indices then openMeld meld else meld)
--                     return $ Just (pair, melds')
--
--         putStrLn "Did the hand have an open wait? [y/n]: "
--         ryanmanWait <- (== "y") <$> getLine
--         shanponWait <-
--             if ryanmanWait
--                 then return False
--                 else do
--                     putStrLn "Did the hand have a dual pair wait? [y/n]: "
--                     (== "y") <$> getLine
--         let goodWait = not (ryanmanWait || shanponWait)
--
--         closedHand <- case maybeIh of
--             Just (_, melds) ->
--                 case numOpen of
--                     0 -> return True
--                     _
--                         | numOpen > 1 -> return False
--                         | otherwise -> case (riichi, tsumo) of
--                             (True, _) -> return True
--                             (False, True) -> return False -- Already know there is an open meld. Now we know it wasn't opened by Ron.
--                             (False, False) ->
--                                 ( do
--                                     putStrLn "Damaten? [y/n]:"
--                                     (== "y") <$> getLine
--                                 )
--               where
--                 numOpen = melds & filter isOpen & length
--             Nothing -> return $ True
--         let (value, yaku_string) = getYaku hand' maybeIh riichi ippatsu tsumo ryanmanWait seatWind roundWind closedHand
--         let fu = case maybeIh of
--                 Just ih ->
--                     if pinfu ih seatWind roundWind ryanmanWait closedHand
--                         then if tsumo then 20 else 30
--                         else getFu ih seatWind roundWind goodWait tsumo closedHand
--                 -- Seven pairs already taken care of, so Nothing signifies thirteen orphans or an invalid hand.
--                 -- So yakuman or invalid - 0 Fu, we will say.
--                 Nothing -> 0
--         putStrLn $ case value of
--             Left (hanClosed, hanOpen) ->
--                 "\tYaku:\n"
--                     ++ yaku_string
--                     ++ "\t\t"
--                     ++ openClosed
--                     ++ toGreen (show (getSum han))
--                     ++ " Han total, with "
--                     ++ toBlue (show fu)
--                     ++ " Fu\n"
--                     ++ "\n\t"
--                     ++ toGreen (show (getScore han fu True tsumo))
--                     ++ " points for Dealer, "
--                     ++ toGreen (show (getScore han fu False tsumo))
--                     ++ " points for Non-Dealer"
--                     ++ ( if name /= ""
--                             then
--                                 " ("
--                                     ++ toMagenta name
--                                     ++ ")."
--                             else ""
--                        )
--               where
--                 han = if (closedHand) then hanClosed else hanOpen
--                 name = hanToHandName han
--                 openClosed = if closedHand then "Closed hand: " else "Open hand: "
--             Right yakumans ->
--                 yaku_string
--                     ++ "\t\t"
--                     ++ toGreen (show (getSum yakumans))
--                     ++ " Yakuman total\n"
--                     ++ "\n\t"
--                     ++ toGreen (show (getSum yakumans * 48000))
--                     ++ " points for Dealer, "
--                     ++ toGreen (show (getSum yakumans * 32000))
--                     ++ " points for Non-Dealer."
