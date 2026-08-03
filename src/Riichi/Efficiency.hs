{- |
Module      : Riichi.Efficiency
Description : Functions for calculating discard tile efficiency
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Efficiency where

import Data.Bifunctor (second)
import Data.Function ((&))
import Data.List
import Riichi.Meld
import Riichi.Shanten
import Riichi.Tile
import Riichi.Waits

{- | Find the optimal discards for a hand that is ready to discard. Here, "optimal" means that the chosen
| discard does not increase the shanten, and among such tiles leaves the most possible improvement tiles
| that can be drawn. An improvement tile is one that reduces shanten.
-}
optimalDiscards :: Hand -> [Tile]
optimalDiscards hand =
    counts
        & filter (\(_, count) -> count == maxCount)
        & map fst
        & sort
        & group
        & map head
  where
    currentShanten = getShanten hand
    removeOne xs = zipWith (++) (inits xs) (map tail (init (tails xs)))
    counts =
        removeOne hand
            & zip hand
            & filter (\(_, hand') -> getShanten hand' <= currentShanten)
            & map (\(tile, hand') -> (tile, countImprovements [tile] hand'))
    maxCount =
        counts
            & maximumBy (\(_, x) (_, y) -> compare x y)
            & snd

-- | Find all tiles that, if added to a supplied hand that is ready to draw, decrease shanten.
improvements :: Hand -> [Tile]
improvements hand = case currentShanten of
    -1 -> []
    0 -> getWaits hand
    _ -> filter (\tile -> currentShanten > getShanten (tile : hand)) allTiles
  where
    currentShanten = getShanten hand

{- | Count how many improvements there are, yet to be drawn, for a given hand. The first input is a (possibly empty) list of
| tiles that have already been discarded. This, together with the tiles in the hand, determines what count is assigned to each
| potential improvement tile.
-}
countImprovements :: Hand -> Hand -> Int
countImprovements alreadyDiscarded hand =
    let
        imps = improvements hand
        nums = [4 - countElem imp notAvailable | imp <- imps]
     in
        sum nums
  where
    countElem x list = length $ filter (== x) list
    notAvailable = hand ++ alreadyDiscarded
