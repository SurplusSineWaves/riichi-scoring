module Meld where

import Data.Function
import Data.List
import Data.Set qualified as Set
import Tile

type Hand = [Tile]

mkHand :: String -> Hand
mkHand tiles = tiles & words & (map readTileBlock) & concat

addDora :: Hand -> Hand -> Hand
addDora [] hand = hand
addDora dora@(doraTile : rest) hand = do
    let hand' = addDora rest hand
    tile <- hand'
    if doraTile == tile
        then return $ case tile of
            (Honour x d) -> (Honour x (d + 1))
            (Numeric s v d) -> (Numeric s v (d + 1))
        else
            return tile

getDora :: Tile -> Dora
getDora (Honour _ d) = d
getDora (Numeric _ _ d) = d

newtype Pair = Pair Tile deriving (Show)

data Meld = Chi Tile Tile Tile Open | Pon Tile Open | Kan Tile Open deriving (Ord)
type Open = Bool

instance Eq Meld where
    (==) (Pon tile1 _) (Pon tile2 _) = tile1 == tile2
    (==) (Kan tile1 _) (Kan tile2 _) = tile1 == tile2
    (==) (Chi tile1 tile2 tile3 _) (Chi tile1' tile2' tile3' _) = Set.fromList ([tile1, tile2, tile3]) == Set.fromList ([tile1', tile2', tile3'])
    (==) _ _ = False

instance Show Meld where
    show (Chi (Numeric suit v1 _) (Numeric _ v2 _) (Numeric _ v3 _) True) = "Open chi: " ++ ((map show $ sort [v1, v2, v3]) & concat) ++ " " ++ (show suit)
    show (Chi (Numeric suit v1 _) (Numeric _ v2 _) (Numeric _ v3 _) False) = "Closed chi: " ++ ((map show $ sort [v1, v2, v3]) & concat) ++ " " ++ (show suit)
    show (Pon (Numeric suit v1 _) True) = "Open pon: " ++ (v1 & show & repeat & (take 3) & concat) ++ " " ++ (show suit)
    show (Pon (Numeric suit v1 _) False) = "Closed pon: " ++ (v1 & show & repeat & (take 3) & concat) ++ " " ++ (show suit)
    show (Kan (Numeric suit v1 _) True) = "Open kan: " ++ (v1 & show & repeat & (take 4) & concat) ++ " " ++ (show suit)
    show (Kan (Numeric suit v1 _) False) = "Closed kan: " ++ (v1 & show & repeat & (take 4) & concat) ++ " " ++ (show suit)
    show (Pon (tile) True) = "Open pon: " ++ (tile & show & repeat & (take 3) & concat)
    show (Pon (tile) False) = "Closed pon: " ++ (tile & show & repeat & (take 3) & concat)
    show (Kan (tile) True) = "Open kan: " ++ (tile & show & repeat & (take 4) & concat)
    show (Kan (tile) False) = "Closed kan: " ++ (tile & show & repeat & (take 4) & concat)

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x : y : ys) = (x == y) && (allEqual (y : ys))

allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent [_] = True
allDifferent (x : xs) = (not (x `elem` xs)) && (allDifferent xs)

isChi :: Tile -> Tile -> Tile -> Bool
isChi (Numeric s1 v1 _) (Numeric s2 v2 _) (Numeric s3 v3 _) =
    (allEqual [s1, s2, s3]) && (Set.fromList (map (subtract m) [v1, v2, v3]) == Set.fromList ([0, 1, 2]))
  where
    m = minimum [v1, v2, v3]
isChi _ _ _ = False

isPon :: Tile -> Tile -> Tile -> Bool
isPon t1 t2 t3 = allEqual [t1, t2, t3]

isKan :: Tile -> Tile -> Tile -> Tile -> Bool
isKan t1 t2 t3 t4 = allEqual [t1, t2, t3, t4]

isOpen :: Meld -> Bool
isOpen (Chi _ _ _ x) = x
isOpen (Pon _ x) = x
isOpen (Kan _ x) = x

isClosed :: Meld -> Bool
isClosed = not . isOpen

openMeld :: Meld -> Meld
openMeld (Chi a b c _) = Chi a b c True
openMeld (Pon a _) = Pon a True
openMeld (Kan a _) = Kan a True

meldIsChi :: Meld -> Bool
meldIsChi (Chi _ _ _ _) = True
meldIsChi _ = False

meldIsPon :: Meld -> Bool
meldIsPon (Pon _ _) = True
meldIsPon _ = False

meldIsKan :: Meld -> Bool
meldIsKan (Kan _ _) = True
meldIsKan _ = False

getMeldBase :: Meld -> Either Integer Honour
getMeldBase (Chi (Numeric _ v1 _) (Numeric _ v2 _) (Numeric _ v3 _) _) = Left (minimum [v1, v2, v3])
getMeldBase (Pon (Numeric _ v1 _) _) = Left v1
getMeldBase (Kan (Numeric _ v1 _) _) = Left v1
getMeldBase (Pon (Honour honour _) _) = Right honour
getMeldBase (Kan (Honour honour _) _) = Right honour

-- This considers each dragon and wind to be its own suit, effectively.
getMeldSuit :: Meld -> Either Suit Honour
getMeldSuit (Chi (Numeric suit _ _) _ _ _) = Left suit
getMeldSuit (Pon (Numeric suit _ _) _) = Left suit
getMeldSuit (Kan (Numeric suit _ _) _) = Left suit
getMeldSuit (Pon (Honour honour _) _) = Right honour
getMeldSuit (Kan (Honour honour _) _) = Right honour

getPairSuit :: Pair -> Either Suit Honour
getPairSuit (Pair (Numeric suit _ _)) = Left suit
getPairSuit (Pair (Honour honour _)) = Right honour

formMelds :: Hand -> [[Meld]]
formMelds [] = [[]]
formMelds [_] = [[]]
formMelds [_, _] = [[]]
formMelds hand@(tile : tiles) =
    let
        -- Form all possible sets of melds with the first meld including the first tile:
        -- triples = tiles & tails & init & (map (\x -> (head x, tail x))) & (map (\(tile2, final_tiles) -> [[tile, tile2, tile3] | tile3 <- final_tiles])) & concat
        triples = do
            (tile2 : rest) <- tiles & tails & init
            tile3 <- rest
            return [tile, tile2, tile3]

        possible_melds =
            ( map
                ( \triple@(tile1 : tile2 : tile3 : _) ->
                    if (isChi tile1 tile2 tile3)
                        then map ((Chi tile1 tile2 tile3 False) :) (formMelds (hand \\ triple))
                        else
                            if (isPon tile1 tile2 tile3)
                                then map ((Pon tile1 False) :) (formMelds (hand \\ triple))
                                else []
                )
                triples
                & concat
            )
                & map sort
                & sort
                & group
                & (map head)
     in
        if possible_melds == []
            then formMelds (tail hand)
            else possible_melds

meldsLength :: [Meld] -> Int
meldsLength [] = 0
meldsLength ((Kan _ _) : rest) = 4 + (meldsLength rest)
meldsLength (_ : rest) = 3 + (meldsLength rest)

concatMelds :: [Meld] -> Hand
concatMelds [] = []
concatMelds (Pon tile _ : rest) = [tile, tile, tile] ++ concatMelds rest
concatMelds (Kan tile _ : rest) = [tile, tile, tile, tile] ++ concatMelds rest
concatMelds (Chi tile1 tile2 tile3 _ : rest) = [tile1, tile2, tile3] ++ concatMelds rest

findPairs :: Hand -> [(Pair, Hand)]
findPairs hand =
    hand
        & sort
        & group
        & (filter (\list -> 2 <= (length list)))
        & (map head)
        & (map (\tile -> (Pair tile, hand \\ [tile, tile])))

findKans :: Hand -> [([Meld], Hand)]
findKans hand =
    hand
        & sort
        & group
        & filter (\list -> 4 == (length list))
        & map head
        & map (\tile -> (Kan tile False))
        & subsequences
        & tail
        & map (\kans -> (kans, hand \\ (concat [tile & repeat & take 4 | Kan tile _ <- kans])))

-- interpretHand assumes the hand consists of a pair and 4 melds (chis pons or kans).
-- An InterpretedHand can then be passed on to other functions to check for yakus.
-- Seven pairs, thirteen orphans etc are handeled in other functions, that should be
-- checked separately.
type InterpretedHand = (Pair, [Meld])
interpretHand :: Hand -> [InterpretedHand]
interpretHand hand =
    let
        possible_pairs = hand & findPairs
        pairs_kans_hands = do
            (pair, hand') <- possible_pairs
            (kans, hand'') <- findKans hand'
            melds <- formMelds hand''
            return (pair, kans ++ melds)
        -- possible_pairs
        --     >>= \(pair, hand') ->
        --         [(pair, kan, hand'') | (kan, hand'') <- findKans hand']
        --             >>= \(pair, kan, hand'') -> [(pair, kan : melds) | melds <- formMelds hand'']
        pairs_hands = do
            (pair, hand') <- possible_pairs
            melds <- formMelds hand'
            return (pair, melds)
     in
        -- possible_pairs
        --     >>= (\(pair, hand') -> [(pair, melds) | melds <- formMelds hand'])

        (pairs_kans_hands ++ pairs_hands)
            -- & (filter (\(_, melds) -> melds /= []))
            & (filter (\(_, melds) -> (length hand) == 2 + (meldsLength melds)))

showInterpretedHand :: InterpretedHand -> String
showInterpretedHand (pair, melds) = (show pair) : (map show melds) & intersperse ", " & concat
