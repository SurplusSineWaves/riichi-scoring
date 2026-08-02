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

improvements :: Hand -> [Tile]
improvements hand = case currentShanten of
    -1 -> []
    0 -> getWaits hand
    _ -> filter (\tile -> currentShanten > getShanten (tile : hand)) allTiles
  where
    currentShanten = getShanten hand

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
