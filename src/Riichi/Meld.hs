{- |
Module      : Riichi.Meld
Description : Datatypes representing melds and associated functions.
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com

A meld is our catch all term for chis, pons and kans.
-}
module Riichi.Meld where

import Data.Function
import Data.List
import Data.Set qualified as Set
import Riichi.Tile

-- | A type alias. A hand is a list of tiles
type Hand = [Tile]

{- | Build a hand from an input string.
| The string should be in the format as described in the help message
| eg: 123p parses to 1 Pin, 2 Pin, 3 Pin; rgNE parses to Red, Green, North, East
-}
mkHand :: String -> Hand
mkHand tiles = tiles & words & (map readTileBlock) & concat

-- | Add dora (in the first argument) a the hand (in the second argument)
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

-- | Get the dora of a given tile
getDora :: Tile -> Dora
getDora (Honour _ d) = d
getDora (Numeric _ _ d) = d

-- | The data for a pair is just a tile
newtype Pair = Pair Tile deriving (Show, Eq)

-- | A meld is a chi, pon, or kan
data Meld = Chi Tile Tile Tile Open | Pon Tile Open | Kan Tile Open deriving (Ord)

-- | Type alias for tracking open / closed melds
type Open = Bool

-- | Almost identical to what deriving Eq would generate, except we consider open and closed melds that are otherwise equal to be the same.
instance Eq Meld where
    (==) (Pon tile1 _) (Pon tile2 _) = tile1 == tile2
    (==) (Kan tile1 _) (Kan tile2 _) = tile1 == tile2
    (==) (Chi tile1 tile2 tile3 _) (Chi tile1' tile2' tile3' _) = Set.fromList ([tile1, tile2, tile3]) == Set.fromList ([tile1', tile2', tile3'])
    (==) _ _ = False

-- | Using the show instance for tile, show melds as "Open/Closed chi/pon/kan: tile tile tile (tile)"
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

-- | Check if all elements of a list of equatable elements are equal
allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual (x : y : ys) = (x == y) && (allEqual (y : ys))

-- | Check if all elements of a list of equatable elements are different
allDifferent :: (Eq a) => [a] -> Bool
allDifferent [] = True
allDifferent [_] = True
allDifferent (x : xs) = (not (x `elem` xs)) && (allDifferent xs)

-- | Check if three tiles form a chi (a sequence)
isChi :: Tile -> Tile -> Tile -> Bool
isChi (Numeric s1 v1 _) (Numeric s2 v2 _) (Numeric s3 v3 _) =
    (allEqual [s1, s2, s3]) && (Set.fromList (map (subtract m) [v1, v2, v3]) == Set.fromList ([0, 1, 2]))
  where
    m = minimum [v1, v2, v3]
isChi _ _ _ = False

-- | Check if three tiles form a pon (a triple)
isPon :: Tile -> Tile -> Tile -> Bool
isPon t1 t2 t3 = allEqual [t1, t2, t3]

-- | Check if four tiles form a kan (a quad)
isKan :: Tile -> Tile -> Tile -> Tile -> Bool
isKan t1 t2 t3 t4 = allEqual [t1, t2, t3, t4]

-- | Check if a meld is open
isOpen :: Meld -> Bool
isOpen (Chi _ _ _ x) = x
isOpen (Pon _ x) = x
isOpen (Kan _ x) = x

-- | Check if a meld is closed
meldIsClosed :: Meld -> Bool
meldIsClosed = not . isOpen

-- | Open a meld
openMeld :: Meld -> Meld
openMeld (Chi a b c _) = Chi a b c True
openMeld (Pon a _) = Pon a True
openMeld (Kan a _) = Kan a True

-- | Check if a meld is a chi
meldIsChi :: Meld -> Bool
meldIsChi (Chi _ _ _ _) = True
meldIsChi _ = False

-- | Check if a meld is a pon
meldIsPon :: Meld -> Bool
meldIsPon (Pon _ _) = True
meldIsPon _ = False

-- | Check if a meld is a kan
meldIsKan :: Meld -> Bool
meldIsKan (Kan _ _) = True
meldIsKan _ = False

{- | Get the "base" of a Meld. For a chi this is the lowest value. Otherwise it is the common value or
| honour instance of the tiles.
-}
getMeldBase :: Meld -> Either Integer Honour
getMeldBase (Chi (Numeric _ v1 _) (Numeric _ v2 _) (Numeric _ v3 _) _) = Left (minimum [v1, v2, v3])
getMeldBase (Pon (Numeric _ v1 _) _) = Left v1
getMeldBase (Kan (Numeric _ v1 _) _) = Left v1
getMeldBase (Pon (Honour honour _) _) = Right honour
getMeldBase (Kan (Honour honour _) _) = Right honour

-- | Get a meld's suit. This considers each dragon and wind to be its own suit.
getMeldSuit :: Meld -> Either Suit Honour
getMeldSuit (Chi (Numeric suit _ _) _ _ _) = Left suit
getMeldSuit (Pon (Numeric suit _ _) _) = Left suit
getMeldSuit (Kan (Numeric suit _ _) _) = Left suit
getMeldSuit (Pon (Honour honour _) _) = Right honour
getMeldSuit (Kan (Honour honour _) _) = Right honour

-- | Get a pair's suit. Treats each dargon and wind as its own suit.
getPairSuit :: Pair -> Either Suit Honour
getPairSuit (Pair (Numeric suit _ _)) = Left suit
getPairSuit (Pair (Honour honour _)) = Right honour

-- | Given a hand, return all the possible ways of interpreting it as a sequence of melds
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

-- | Count the tiles in a list of melds
meldsLength :: [Meld] -> Int
meldsLength [] = 0
meldsLength ((Kan _ _) : rest) = 4 + (meldsLength rest)
meldsLength (_ : rest) = 3 + (meldsLength rest)

-- | Reverse form melds. Concatenate a list of melds back into a hand.
concatMelds :: [Meld] -> Hand
concatMelds [] = []
concatMelds (Pon tile _ : rest) = [tile, tile, tile] ++ concatMelds rest
concatMelds (Kan tile _ : rest) = [tile, tile, tile, tile] ++ concatMelds rest
concatMelds (Chi tile1 tile2 tile3 _ : rest) = [tile1, tile2, tile3] ++ concatMelds rest

-- | Find the unique pairs in a hand. Returns a list of pairs, each along with the remaining tiles in the hand not in the pair.
findPairs :: Hand -> [(Pair, Hand)]
findPairs hand =
    hand
        & sort
        & group
        & (filter (\list -> 2 <= (length list)))
        & (map head)
        & (map (\tile -> (Pair tile, hand \\ [tile, tile])))

-- | Find all the ways of pulling kans out of a hand. In each case pair the set of kans with what remains of the hand
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

-- | A pair and four melds, constituting a complete hand
type InterpretedHand = (Pair, [Meld])

-- | Find all the ways of interpreting a hand as a pair and four melds.
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

-- | Show a full interpreted hand
showInterpretedHand :: InterpretedHand -> String
showInterpretedHand (pair, melds) = (show pair) : (map show melds) & intersperse ", " & concat

-- | Ask which meld was opened by ron. Return modified melds with that meld opened.
getRonMeld :: [Meld] -> IO [Meld]
getRonMeld melds = do
    putStrLn "Which meld was opened by Ron? (leave blank if it was the pair): "
    sequence_ $ [("[" ++ show i ++ "]: " ++ (meld & show)) & putStrLn | (i :: Integer, meld) <- zip [0 ..] melds]
    input <- getLine
    if input == ""
        then return melds
        else
            let index :: Int = input & read
             in return $ (zip [0 ..] melds) & map (\(i, meld) -> if i == index then openMeld meld else meld)

{- | Ask which melds are open in a set of melds. Return modified melds with this data added. Opens the specified melds,
| but if a meld is already open it stays that way. (This may change in the future)
-}
getOpenMelds :: [Meld] -> IO [Meld]
getOpenMelds melds = do
    putStrLn "Which melds are open? (enter a string of indices, or leave blank if all closed): "
    sequence_ $ [("[" ++ show i ++ "]: " ++ (meld & show)) & putStrLn | (i :: Integer, meld) <- zip [0 ..] melds]
    input <- getLine
    let indices :: [Int] = input & map return & (map read)
    return $ (zip [0 ..] melds) & map (\(i, meld) -> if i `elem` indices then openMeld meld else meld)
