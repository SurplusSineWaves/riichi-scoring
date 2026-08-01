{- |
Module      : Riichi.Shanten
Description : Datatypes representing tiles and associated functions.
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

data Taatsu = Taatsu Tile Tile deriving (Show, Eq, Ord)

type Partial = Either Taatsu Pair

getShanten :: Hand -> Int
getShanten hand = minimum $ do
    let meldss = formMelds hand
    melds <- meldss
    let partialss = formPartials (hand \\ concatMelds melds)
    partials <- partialss
    let m = length melds
    let (t, p) = countTatsuPairs partials
    return $ 8 - (2 * m) - min (t + p) (4 - m) - (if p >= 1 && (m + t + p >= 5) then 1 else 0)

countTatsuPairs :: [Partial] -> (Int, Int)
countTatsuPairs partials = (numTatsu, numPairs)
  where
    num = length partials
    numTatsu = length $ lefts partials
    numPairs = num - numTatsu

formPartials :: Hand -> [[Partial]]
formPartials [] = [[]]
formPartials [_] = [[]]
formPartials hand@(tile1 : hand') =
    let
        -- Get all sets of 2 tiles, including the first tile
        doubles = do
            tile2 <- hand'
            return [tile1, tile2]
        partials =
            formPartials (tail hand) ++ do
                [tile1, tile2] <- doubles
                partial <- maybeToList $ mkPartial tile1 tile2
                map (partial :) $ formPartials (hand' \\ [tile2])
     in
        partials
            & sortBy (\x y -> compare (length x) (length y))
            & map sort
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
