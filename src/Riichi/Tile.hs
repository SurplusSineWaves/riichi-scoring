module Riichi.Tile where

import Data.Function
import Data.List (intersperse)

data Tile = Honour Honour Dora | Numeric Suit Value Dora deriving (Ord)
type Dora = Integer
type Value = Integer

data Honour = Dragon (Dragon) | Wind (Wind) deriving (Show, Eq, Ord)
data Dragon = White | Green | Red deriving (Show, Eq, Ord)
data Wind = North | East | South | West deriving (Show, Eq, Ord)

data Suit = Man | Pin | Sou deriving (Show, Eq, Ord)

-- instance Show Value where
--     show (Value int) = show int
--
-- instance Ord Value where
--     compare (Value a) (Value b) = compare a b

instance Read Tile where
    readsPrec :: Int -> ReadS Tile
    readsPrec _ (x : []) = [(tile, [])]
      where
        tile = case x of
            'N' -> (Honour $ Wind $ North) 0
            'E' -> (Honour $ Wind $ East) 0
            'S' -> (Honour $ Wind $ South) 0
            'W' -> (Honour $ Wind $ West) 0
            'r' -> (Honour $ Dragon $ Red) 0
            'g' -> (Honour $ Dragon $ Green) 0
            'w' -> (Honour $ Dragon $ White) 0
    readsPrec _ (x : y : _) = do
        let ((value, dora), suit) =
                ( case x of
                    '0' -> (5, 1)
                    '1' -> (1, 0)
                    '2' -> (2, 0)
                    '3' -> (3, 0)
                    '4' -> (4, 0)
                    '5' -> (5, 0)
                    '6' -> (6, 0)
                    '7' -> (7, 0)
                    '8' -> (8, 0)
                    '9' -> (9, 0)
                , case y of
                    'p' -> Pin
                    'm' -> Man
                    's' -> Sou
                )
         in [(Numeric suit value dora, [])]

readTileBlock :: String -> [Tile]
readTileBlock string =
    let
        final = last string
        (stripped, separator) = case final of
            'p' -> (init string, "p ")
            'm' -> (init string, "m ")
            's' -> (init string, "s ")
            _ -> (string, " ")
     in
        stripped & map (\c -> [c]) & intersperse separator & concat & (++ separator) & words & map read

instance Show Tile where
    show (Honour (Dragon Red) _) = "r"
    show (Honour (Dragon Green) _) = "g"
    show (Honour (Dragon White) _) = "w"
    show (Honour (Wind North) _) = "N"
    show (Honour (Wind East) _) = "E"
    show (Honour (Wind South) _) = "S"
    show (Honour (Wind West) _) = "W"
    show (Numeric Pin 5 d) = if d == 0 then "5p" else "5p*"
    show (Numeric Man 5 d) = if d == 0 then "5m" else "5m*"
    show (Numeric Sou 5 d) = if d == 0 then "5s" else "5s*"
    show (Numeric Pin n _) = (show n) ++ "p"
    show (Numeric Man n _) = (show n) ++ "m"
    show (Numeric Sou n _) = (show n) ++ "s"

-- We don't care about dora when equating tiles
instance Eq Tile where
    (==) (Honour honour _) (Honour honour' _) = (honour == honour')
    (==) (Numeric suit value _) (Numeric suit' value' _) = (suit == suit') && (value == value')
    (==) _ _ = False

getTileSuit :: Tile -> Either Suit Honour
getTileSuit (Numeric Pin _ _) = Left Pin
getTileSuit (Numeric Man _ _) = Left Man
getTileSuit (Numeric Sou _ _) = Left Sou
getTileSuit (Honour honour _) = Right honour

isSimple :: Tile -> Bool
isSimple (Honour _ _) = False
isSimple (Numeric _ 1 _) = False
isSimple (Numeric _ 9 _) = False
isSimple _ = True

isTerminal :: Tile -> Bool
isTerminal (Numeric _ 1 _) = True
isTerminal (Numeric _ 9 _) = True
isTerminal _ = False

isHonour :: Tile -> Bool
isHonour (Honour _ _) = True
isHonour _ = False

isNumeric :: Tile -> Bool
isNumeric = not . isHonour

isDragon :: Tile -> Bool
isDragon (Honour (Dragon _) _) = True
isDragon _ = False

isWind :: Tile -> Bool
isWind (Honour (Wind _) _) = True
isWind _ = False

honourIsDragon :: Honour -> Bool
honourIsDragon (Dragon _) = True
honourIsDragon _ = False

honourIsWind :: Honour -> Bool
honourIsWind = not . honourIsDragon
