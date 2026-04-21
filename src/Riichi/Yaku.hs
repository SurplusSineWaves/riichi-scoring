{- |
Module      : Riichi.Yaku
Description : Methods for detecting the various yaku conditions
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Yaku where

import Data.Either (isLeft, lefts, rights)
import Data.Function
import Data.List
import Data.Set qualified as Set
import Riichi.Meld
import Riichi.Tile

-- Note that Yaku functions that operate on Hands needn't check that the
-- hand is actually valid to begin with. This should be done by the caller,
-- by seeing if interpretHand returns an nonempty list of interpretations.
-- Some of these could probably be rephrased to be point free but I think
-- that would just make them more confusing.

-- | Check a hand for tanyao, aka all simples
tanyao :: Hand -> Bool
tanyao hand = hand & (map isSimple) & and

{- | Check if a hand is composed entirely of unique pairs.
Note this doesn't check the hand has the right size to be seven pairs.
-}
allPairs :: Hand -> Bool
allPairs hand = (hand & findPairs & length) * 2 == length hand

-- | Check a hand for the yaku seven pairs
chiitoitsu :: Hand -> Bool
chiitoitsu hand = (allPairs hand) && (length hand == 14)

-- | Check if a hand is thirteen orphans, a yakuman.
thirteenOrphans :: Hand -> Bool
thirteenOrphans hand =
    (length hand == 14)
        && ( Set.fromList hand
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

-- | Counts the number of yakuhai dragon triplets. One han each.
yakuhaiDragons :: InterpretedHand -> Int
yakuhaiDragons (_, melds) =
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

-- | Check for the presence of a triplet in an interpreted hand.
checkPon :: Tile -> InterpretedHand -> Bool
checkPon tile (_, melds) = Pon tile False `elem` melds

-- | Check for the presence of a white dragon triplet in an interpreted hand.
haku :: InterpretedHand -> Bool
haku = checkPon ((Honour $ Dragon $ White) 0)

-- | Check for the presence of a green dragon triplet in an interpreted hand.
hatsu :: InterpretedHand -> Bool
hatsu = checkPon ((Honour $ Dragon $ Green) 0)

-- | Check for the presence of a red dragon triplet in an interpreted hand.
chun :: InterpretedHand -> Bool
chun = checkPon ((Honour $ Dragon $ Red) 0)

-- | Check for the presence of a north wind triplet in an interpreted hand.
checkNorth :: InterpretedHand -> Bool
checkNorth = checkPon ((Honour $ Wind $ North) 0)

-- | Check for the presence of a east wind triplet in an interpreted hand.
checkEast :: InterpretedHand -> Bool
checkEast = checkPon ((Honour $ Wind $ East) 0)

-- | Check for the presence of a south wind triplet in an interpreted hand.
checkSouth :: InterpretedHand -> Bool
checkSouth = checkPon ((Honour $ Wind $ South) 0)

-- | Check for the presence of a west wind triplet in an interpreted hand.
checkWest :: InterpretedHand -> Bool
checkWest = checkPon ((Honour $ Wind $ West) 0)

-- | Check for the presence of a given wind triplet in an interpreted hand.
checkWind :: Wind -> InterpretedHand -> Bool
checkWind East = checkEast
checkWind North = checkNorth
checkWind West = checkWest
checkWind South = checkSouth

-- | Check for the same sequence in all three suit
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

-- | Check for the same triplet (or quad) in all three suits
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

-- | Check if a hand is a full flush
chinitsu :: Hand -> Bool
chinitsu hand = (hand & map getTileSuit & allEqual) && (hand & head & getTileSuit & isLeft)

-- | Check if a hand is a half flush
honitsu :: Hand -> Bool
-- Only check equality on the lefts of Either Suit Honour, i.e the suited melds.
honitsu hand = hand & map getTileSuit & lefts & allEqual

-- | Check if a hand is all triplets
toitoi :: InterpretedHand -> Bool
toitoi (_, melds) = melds & (map (\meld -> meldIsPon meld || meldIsKan meld)) & and

-- | Check if a hand has 1-9 in a single suit
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

-- | Check a hand for three quads (open or closed)
sankantsu :: InterpretedHand -> Bool
sankantsu (_, melds) = melds & (filter meldIsKan) & length & (3 ==)

-- | Check a hand for four quads (open or closed). Yakuman
suukantsu :: InterpretedHand -> Bool
suukantsu (_, melds) = melds & (filter meldIsKan) & length & (4 ==)

-- Little three dragons. Worth noting that we permit ourselves to assume that hands
-- don't contain more than 4 of a given tile! So no need to worry about multiple melds
-- of the same dragon.

-- | Check a hand for little three dragons
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

-- | Check a hand for big three dragons. Yakuman
daisangen :: InterpretedHand -> Bool
daisangen (_, melds) =
    ( melds
        & (map getMeldSuit)
        & rights
        & (filter honourIsDragon)
        & length
        & (3 ==)
    )

-- | Check a hand for little winds. Yakuman
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

-- | Check a hand for big winds. Double Yakuman
daisuushii :: InterpretedHand -> Bool
daisuushii (_, melds) =
    ( melds
        & (map getMeldSuit)
        & rights
        & (filter honourIsWind)
        & length
        & (4 ==)
    )

-- | Check for pure double sequence. Closed only, but this function doesn't check that.
iipeikou :: InterpretedHand -> Bool
iipeikou (_, melds) = melds & filter meldIsChi & sort & group & map length & filter (< 4) & filter (>= 2) & length & (== 1)

-- | Check for twice pure double sequence. Closed only, but this function doesn't check that.
ryanpeikou :: InterpretedHand -> Bool
ryanpeikou (_, melds) = melds & filter meldIsChi & sort & group & map length & filter (< 4) & filter (>= 2) & length & (>= 2)

-- | Check for half outside hand
chanta :: InterpretedHand -> Bool
chanta (Pair tile, melds) = (melds & map getMeldBase & lefts & filter (\x -> x /= 1 && x /= 7)) == [] && (not $ isSimple tile)

-- | Check for fully outside hand (chanta + no honnours)
junchan :: InterpretedHand -> Bool
junchan ih@(Pair tile, melds) = (isNumeric tile) && (melds & map getMeldBase & rights) == [] && (chanta ih)

-- | Check for all terminals and honours
honroutou :: Hand -> Bool
honroutou hand = hand & map (\tile -> isHonour tile || isTerminal tile) & and

{- | Check for all honours. Yakuman.
Does not check that the hand is valid to begin with.
-}
tsuuiisou :: Hand -> Bool
tsuuiisou hand = hand & map isHonour & and

{- | Check for all terminals. Yakuman.
Does not check that the hand is valid to begin with.
-}
chinroutou :: Hand -> Bool
chinroutou hand = hand & map isTerminal & and

{- | Check for all green. Yakuman.
Does not check that the hand is valid to begin with.
-}
ryuuiisou :: Hand -> Bool
ryuuiisou hand = hand & map isGreen & and
  where
    isGreen (Numeric Sou v _) = (v `elem` [2, 3, 4, 6, 8])
    isGreen (Honour (Dragon Green) _) = True
    isGreen _ = False

{- | Check for nine gates. Yakuman.
Does not check that the hand is valid to begin with, nor that it is closed.
(Indeed, checking closure of the hand would require more information, thus complicating the function signature).
Note that kans are dissalowed by rule. A valid hand + length == 14 (which this function checks) ensures this.
-}
chuurenPoutou :: Hand -> Bool
chuurenPoutou hand = (chinitsu hand) && (length list == 9) && (head list >= 3) && (last list >= 3) && (length hand == 14)
  where
    list = (hand & sort & group & map length)

-- | Check for three concealed triplets. Note that a closed hand may have a triplet that is not concealed, if won by ron.
sanankou :: InterpretedHand -> Bool
sanankou (_, melds) = (melds & filter (not . meldIsChi) & filter (meldIsClosed) & length) == 3

-- | Check for four concealed triplets. Yakuman.
suuankou :: InterpretedHand -> Bool
suuankou (_, melds) = (melds & filter (not . meldIsChi) & filter (meldIsClosed) & length) == 4

-- | Check for pinfu. Takes the hand, the seat wind, round wind, whether the wait was ryanman, and whether it is closed (in that order).
pinfu :: InterpretedHand -> Wind -> Wind -> Bool -> Bool -> Bool
pinfu (Pair tile, melds) seatWind roundWind ryanmanWait closedHand =
    (melds & filter (not . meldIsChi)) == []
        && closedHand
        && (not $ isDragon tile)
        && (tile /= (Honour (Wind seatWind) 0))
        && (tile /= (Honour (Wind roundWind) 0))
        && ryanmanWait
