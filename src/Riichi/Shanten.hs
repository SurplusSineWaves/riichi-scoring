{- |
Module      : Riichi.Shanten
Description : Datatypes and functions for calculating Shanten
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Shanten where

import Data.Either (lefts)
import Data.Function ((&))
import Data.List
import Data.Maybe
import Riichi.Meld
import Riichi.Tile
import Text.ParserCombinators.ReadP (count)

data Taatsu = Taatsu Tile Tile deriving (Show, Eq, Ord)

type Partial = Either Taatsu Pair

getShanten :: Hand -> Int
getShanten hand
    | length hand `elem` [13, 14] = if 0 `elem` list then 0 else minimum list
    | otherwise = basicShanten hand
  where
    -- Postpone basic shanten calculation until after we know the other two (which are faster) are not 0
    p = pairsShanten hand
    o = orphansShanten hand
    b = basicShanten hand
    list = [p, o, b]

basicShanten :: Hand -> Int
basicShanten hand = minimum $ do
    -- The combinatorics can blow up here, especially on a hand like 1111p 2222p 3333p 4444p rr
    -- Need to try to prune out as many cases as we can, and we must dispose of cases that find so
    -- many taatsu or melds that shanten is negative!
    let meldss = formMelds' hand
    melds <- meldss
    let m = length melds
    -- Don't bother if the number of partials pushes us past 5 blocks
    let partialss = splitAcrossSuits (formPartials (5 - m)) (hand \\ concatMelds melds)
    partials <- partialss
    let (t, p) = countTatsuPairs partials
    return $ 8 - (2 * m) - min (t + p) (4 - m) - (if p >= 1 && (m + t + p >= 5) then 1 else 0)

pairsShanten :: Hand -> Int
pairsShanten hand = 6 - numPairs + max 0 (7 - uniqueTiles)
  where
    numPairs = length $ findPairs hand
    uniqueTiles = length $ map head $ group $ sort hand

orphansShanten :: Hand -> Int
orphansShanten hand = 13 - uniqueOrphans - pairs
  where
    orphans = filter (not . isSimple) hand
    groupedOrphans = group $ sort orphans
    uniqueOrphans = length $ map head groupedOrphans
    pairs = if any ((>= 2) . length) groupedOrphans then 1 else 0

countTatsuPairs :: [Partial] -> (Int, Int)
countTatsuPairs partials = (numTatsu, numPairs)
  where
    num = length partials
    numTatsu = length $ lefts partials
    numPairs = num - numTatsu

formPartials :: Int -> Hand -> [[Partial]]
formPartials 0 _ = [[]]
formPartials _ [] = [[]]
formPartials _ [_] = [[]]
formPartials n hand@(tile1 : hand') =
    let
        -- Get all sets of 2 tiles, including the first tile
        doubles = do
            tile2 <- hand'
            return [tile1, tile2]
        partials =
            formPartials n (tail hand) ++ do
                [tile1, tile2] <- doubles
                partial <- maybeToList $ mkPartial tile1 tile2
                map (partial :) $ formPartials (n - 1) (hand' \\ [tile2])
     in
        partials
            -- & sortBy (\x y -> compare (length x) (length y))
            & map sort
            & sort
            & group
            & map head

mkPartial :: Tile -> Tile -> Maybe Partial
mkPartial tile1@(Numeric suit1 val1 _) tile2@(Numeric suit2 val2 _)
    | suit1 /= suit2 = Nothing
    | val1 == val2 = Just $ Right $ Pair tile1
    | abs (val1 - val2) <= 2 =
        if val1 <= val2
            then Just $ Left $ Taatsu tile1 tile2
            else Just $ Left $ Taatsu tile2 tile1
    | otherwise = Nothing
mkPartial tile1@(Honour _ _) tile2@(Honour _ _) =
    if tile1 == tile2
        then Just $ Right $ Pair tile1
        else Nothing
mkPartial _ _ = Nothing
