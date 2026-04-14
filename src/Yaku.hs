{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Yaku where

import Data.Either (lefts, rights)
import Data.Function
import Data.List
import Data.Set qualified as Set
import Meld
import Tile

-- Note that Yaku functions that operate on Hands needn't check that the
-- hand is actually valid to begin with. This should be done by the caller,
-- by seeing if interpretHand returns an nonempty list of interpretations.
-- Some of these could probably be rephrased to be point free but I think
-- that would just make them more confusing.

tanyao :: Hand -> Bool
tanyao hand = hand & (map isSimple) & and

allPairs :: Hand -> Bool
allPairs hand = (hand & findPairs & length) * 2 == length hand

-- Yakuman
thirteenOrphans :: Hand -> Bool
thirteenOrphans hand =
    ( Set.fromList hand
        == Set.fromList
            [ (Honour $ Dragon $ Red) 0
            , (Honour $ Dragon $ White) 0
            , (Honour $ Dragon $ Green) 0
            , (Honour $ Wind $ North) 0
            , (Honour $ Wind $ South) 0
            , (Honour $ Wind $ East) 0
            , (Honour $ Wind $ West) 0
            , (Numeric Pin 1) 0
            , (Numeric Pin 9) 0
            , (Numeric Man 1) 0
            , (Numeric Man 9) 0
            , (Numeric Sou 1) 0
            , (Numeric Sou 9) 0
            ]
    )
        && (length hand == 14)

-- Counts the number of yakuhai pairs. One han each.
yakuhai :: InterpretedHand -> Int
yakuhai (_, melds) =
    melds
        & ( filter
                ( \meld ->
                    meld
                        `elem` [ Pon ((Honour $ Dragon $ Red) 0) False
                               , Pon ((Honour $ Dragon $ Green) 0) False
                               , Pon ((Honour $ Dragon $ White) 0) False
                               ]
                )
          )
        & length

checkPon :: Tile -> InterpretedHand -> Bool
checkPon tile (_, melds) = Pon tile False `elem` melds

haku :: InterpretedHand -> Bool
haku = checkPon ((Honour $ Dragon $ White) 0)

hatsu :: InterpretedHand -> Bool
hatsu = checkPon ((Honour $ Dragon $ Green) 0)

chun :: InterpretedHand -> Bool
chun = checkPon ((Honour $ Dragon $ Red) 0)

checkNorth :: InterpretedHand -> Bool
checkNorth = checkPon ((Honour $ Wind $ North) 0)

checkEast :: InterpretedHand -> Bool
checkEast = checkPon ((Honour $ Wind $ East) 0)

checkSouth :: InterpretedHand -> Bool
checkSouth = checkPon ((Honour $ Wind $ South) 0)

checkWest :: InterpretedHand -> Bool
checkWest = checkPon ((Honour $ Wind $ West) 0)

checkWind :: Wind -> InterpretedHand -> Bool
checkWind East = checkEast
checkWind North = checkNorth
checkWind West = checkWest
checkWind South = checkSouth

-- Same sequence in all three suits
sanshokuDoujun :: InterpretedHand -> Bool
sanshokuDoujun (_, melds) =
    let
        chis = melds & (filter meldIsChi)
        suits = chis & (map getMeldSuit)
        bases = chis & (map getMeldBase)
        suits_bases = zip suits bases
     in
        or
            [ ((Left Man, Left base) `elem` suits_bases)
                && ((Left Pin, Left base) `elem` suits_bases)
                && ((Left Sou, Left base) `elem` suits_bases)
            | base <- [1 .. 7]
            ]

-- Same triplet (or Kan!) in all three suits
sanshokuDoukou :: InterpretedHand -> Bool
sanshokuDoukou (_, melds) =
    let
        pons = melds & (filter (\meld -> meldIsPon meld || meldIsKan meld))
        suits = pons & (map getMeldSuit)
        bases = pons & (map getMeldBase)
        suits_bases = zip suits bases
     in
        or
            [ ((Left Man, Left base) `elem` suits_bases)
                && ((Left Pin, Left base) `elem` suits_bases)
                && ((Left Sou, Left base) `elem` suits_bases)
            | base <- [1 .. 9]
            ]

-- Full flush
-- chinitsu :: InterpretedHand -> Bool
-- chinitsu (pair, melds) = allEqual ((getPairSuit pair) : (melds & (map getMeldSuit)))
chinitsu :: Hand -> Bool
chinitsu hand = hand & map getTileSuit & allEqual

-- Half flush
-- Only check equality on the lefts of Either Suit Honour, i.e the suited melds.
-- honitsu :: InterpretedHand -> Bool
-- honitsu (pair, melds) =
--     ((getPairSuit pair) : (melds & (map getMeldSuit)))
--         & lefts
--         & allEqual
honitsu :: Hand -> Bool
honitsu hand = hand & map getTileSuit & lefts & allEqual

toitoi :: InterpretedHand -> Bool
toitoi (_, melds) = melds & (map (\meld -> meldIsPon meld || meldIsKan meld)) & and

ittsuu :: InterpretedHand -> Bool
ittsuu (_, melds) =
    let
        chis = melds & (filter meldIsChi)
        suits = chis & (map getMeldSuit)
        bases = chis & (map getMeldBase)
        suits_bases = zip suits bases
     in
        or
            [ ((Left suit, Left 1) `elem` suits_bases)
                && ((Left suit, Left 4) `elem` suits_bases)
                && ((Left suit, Left 7) `elem` suits_bases)
            | suit <- [Man, Pin, Sou]
            ]

-- Three quads (open or closed)
sankantsu :: InterpretedHand -> Bool
sankantsu (_, melds) = melds & (filter meldIsKan) & length & (3 ==)

-- Four quads (open or closed). Yakuman
suukantsu :: InterpretedHand -> Bool
suukantsu (_, melds) = melds & (filter meldIsKan) & length & (4 ==)

-- Little three dragons. Worth noting that we permit ourselves to assume that hands
-- don't contain more than 4 of a given tile! So no need to worry about multiple melds
-- of the same dragon.
shousangen :: InterpretedHand -> Bool
shousangen (Pair tile, melds) =
    (isDragon tile)
        && ( melds
                & (map getMeldSuit)
                & rights
                & (filter honourIsDragon)
                & length
                & (2 ==)
           )

-- Big three dragons. Yakuman
daisangen :: InterpretedHand -> Bool
daisangen (_, melds) =
    ( melds
        & (map getMeldSuit)
        & rights
        & (filter honourIsDragon)
        & length
        & (3 ==)
    )

-- Little winds. Yakuman
shousuushii :: InterpretedHand -> Bool
shousuushii (Pair tile, melds) =
    (isWind tile)
        && ( melds
                & (map getMeldSuit)
                & rights
                & (filter honourIsWind)
                & length
                & (3 ==)
           )

-- Big winds. Double Yakuman
daisuushii :: InterpretedHand -> Bool
daisuushii (_, melds) =
    ( melds
        & (map getMeldSuit)
        & rights
        & (filter honourIsWind)
        & length
        & (4 ==)
    )

-- Pure double sequence. Closed only!
iipeikou :: InterpretedHand -> Bool
iipeikou (_, melds) = melds & filter meldIsChi & sort & group & map length & filter (< 4) & filter (>= 2) & length & (== 1)

-- Twice pure double sequence. Note we require the two pairs of sequences to be distinct.
ryanpeikou :: InterpretedHand -> Bool
ryanpeikou (_, melds) = melds & filter meldIsChi & sort & group & map length & filter (< 4) & filter (>= 2) & length & (>= 2)

-- Half outside hand
chanta :: InterpretedHand -> Bool
chanta (Pair tile, melds) = (melds & map getMeldBase & lefts & filter (\x -> x /= 1 && x /= 7)) == [] && (not $ isSimple tile)

-- Fully outside hand (chanta + no honours)
junchan :: InterpretedHand -> Bool
junchan ih@(Pair tile, melds) = (isNumeric tile) && (melds & map getMeldBase & rights) == [] && (chanta ih)

-- All terminals and honours
honroutou :: Hand -> Bool
honroutou hand = hand & map (\tile -> isHonour tile || isTerminal tile) & and

-- All honours. Yakuman
tsuuiisou :: Hand -> Bool
tsuuiisou hand = hand & map isHonour & and

-- All terminals. Yakuman
chinroutou :: Hand -> Bool
chinroutou hand = hand & map isTerminal & and

-- All green. Yakuman
ryuuiisou :: Hand -> Bool
ryuuiisou hand = hand & map isGreen & and
  where
    isGreen (Numeric Sou v _) = (v `elem` [2, 3, 4, 6, 8])
    isGreen (Honour (Dragon Green) _) = True
    isGreen _ = False

-- Nine Gates. Yakuman
-- Length == 9 precludes the possibility of a all honours chinitsu.
chuurenPoutou :: Hand -> Bool
chuurenPoutou hand = (chinitsu hand) && (length list == 9) && (head list >= 3) && (last list >= 3)
  where
    list = (hand & sort & group & map length)

-- Three concealed triplets
sanankou :: InterpretedHand -> Bool
sanankou (_, melds) = (melds & filter (not . meldIsChi) & filter (isClosed) & length) == 3

-- Four concealed triplets
suuankou :: InterpretedHand -> Bool
suuankou (_, melds) = (melds & filter (not . meldIsChi) & filter (isClosed) & length) == 4

pinfu :: InterpretedHand -> Wind -> Wind -> Bool -> Bool -> Bool
pinfu (Pair tile, melds) seatWind roundWind ryanmanWait closedHand =
    (melds & filter (not . meldIsChi)) == []
        && closedHand
        && (not $ isDragon tile)
        && (tile /= (Honour (Wind seatWind) 0))
        && (tile /= (Honour (Wind roundWind) 0))
        && ryanmanWait
