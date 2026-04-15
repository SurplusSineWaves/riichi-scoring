module Riichi.Waits where

import Data.Function
import Data.List
import Riichi.Meld
import Riichi.Tile

getWaits :: Hand -> [Tile]
getWaits hand =
    if length hand < 13
        then []
        else
            -- Check seven pairs
            let pairs = findPairs hand
                sevenPairsWait =
                    ( if length pairs == 6
                        then case (hand & sort & group & filter (\gp -> 1 == length gp)) of
                            [[tile]] -> [tile]
                            _ -> []
                        else []
                    )
                -- Check thirteen orphans
                orphansWaits =
                    if length hand == 13
                        then do
                            let orphans = (mkHand "1p 9p 1s 9s 1m 9m N E S W r g w")
                            let difference = hand \\ orphans
                            case difference of
                                [] -> orphans
                                [tile] | tile `elem` orphans -> orphans \\ hand
                                _ -> []
                        else []
                -- Check waits from standard interpretations
                -- If we have four melds already, a single remaning tile can wait for a pair
                possibleMelds = formMelds hand
                fourMelds = possibleMelds & filter (\melds -> length melds == 4)
                fourMeldsWaits = fourMelds & map (concatMelds) & map (hand \\) & filter (\diff -> length diff == 1) & concat
                -- Finally, we might be waiting with three melds and a pair
                threeMeldWaits = concat $ do
                    (pair, hand') <- findPairs hand
                    melds <- formMelds hand'
                    if length melds /= 3
                        then return []
                        else
                            let diff = hand' \\ (concatMelds melds)
                             in if length diff /= 2
                                    then return []
                                    else let [a, b] = diff in return (meldWait a b)
             in (sevenPairsWait ++ orphansWaits ++ fourMeldsWaits ++ threeMeldWaits) & sort & group & map head

meldWait :: Tile -> Tile -> [Tile]
meldWait a b
    | a == b = [a]
meldWait t1@(Numeric s1 v1 _) t2@(Numeric s2 v2 _)
    | v1 > v2 = meldWait t2 t1
    | s1 /= s2 = []
    -- \| v1 == v2 = [Numeric s1 v1 0]
    | v1 + 1 == v2 =
        if v1 == 1
            then [Numeric s1 (v2 + 1) 0]
            else
                if v2 == 9
                    then [Numeric s1 (v1 - 1) 0]
                    else [Numeric s1 (v1 - 1) 0, Numeric s1 (v2 + 1) 0]
    | v1 + 2 == v2 = [Numeric s1 (v1 + 1) 0]
meldWait _ _ = []
