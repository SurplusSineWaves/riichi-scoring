{- |
Module      : Riichi.Shanten
Description : Datatypes representing tiles and associated functions.
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Shanten where

import Data.Function ((&))
import Data.List
import Data.Maybe
import Riichi.Meld
import Riichi.Tile

data Taatsu = Taatsu Tile Tile deriving (Show, Eq, Ord)

type Partial = Either Taatsu Pair

shanten :: Hand -> Int
shanten hand = undefined

formPartials :: Hand -> [[Partial]]
formPartials hand = do
    let partials = formPartials' hand
    partials
        & sort
        & group
        & map head
        & sortBy (\x y -> compare (length x) (length y))
  where
    formPartials' [] = [[]]
    formPartials' [_] = [[]]
    formPartials' hand@(tile1 : hand') = do
        tile2 <- hand'
        partial <- maybeToList $ mkPartial tile1 tile2
        let nextPartials = formPartials' (hand' \\ [tile2])
        formPartials' (tail hand) ++ map (partial :) nextPartials

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

-- mkPartial tile1 tile2
--     | tile1 == tile2 = Just $ Right $ Pair tile1
--     | isHonour tile1 || isHonour tile2 = Nothing
--     | otherwise = undefined

-- if tile1 == tile2
--     then
--         Just $ Right $ Pair tile1
--     else
--         if isHonour tile1 || isHonour tile2
--             then
--                 Nothing
--             else
--                 undefined
