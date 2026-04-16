module Riichi.Context where

import Riichi.Meld (Hand, InterpretedHand, mkHand)
import Riichi.Tile
import Riichi.Yaku

askYesNo :: String -> IO Bool
askYesNo string = do
    putStrLn string
    input <- getLine
    if input == "y" then return True else return False

{- | Record to track the additional context for a hand. Fields with a Maybe type are information that
we may not need in order to fully understand the hand.
-}
data HandContext = HandContext
    { isClosed :: Maybe Bool
    , isTsumo :: Maybe Bool
    , riichi :: Maybe RiichiContext
    , wait :: Maybe WaitContext
    , wind :: Maybe WindContext
    , isSevenPairs :: Bool
    , isThirteenOrphans :: Bool
    }

getMinimalHandContext :: Hand -> Maybe InterpretedHand -> HandContext
getMinimalHandContext hand Nothing =
    let
        sevenPairs = chiitoitsu hand
        orphans = thirteenOrphans hand
     in
        HandContext
            { isClosed = Nothing
            , isTsumo = Nothing
            , riichi = Nothing
            , wait = Nothing
            , wind = Nothing
            , isSevenPairs = sevenPairs
            , isThirteenOrphans = orphans
            }

addRiichiContext :: HandContext -> IO HandContext
addRiichiContext handContext = do
    riichiContext <- askRiichiContext
    return handContext{riichi = Just riichiContext}

addWaitContext :: HandContext -> IO HandContext
addWaitContext handContext = do
    waitContext <- askWaitContext
    return handContext{wait = Just waitContext}

addWindContext :: HandContext -> IO HandContext
addWindContext handContext = do
    windContext <- askWindContext
    return handContext{wind = Just windContext}

addClosedContext :: HandContext -> IO HandContext
addClosedContext = undefined

addTsumoContext :: HandContext -> IO HandContext
addTsumoContext = undefined

data WaitContext = WaitContext
    { isRyanmanWait :: Bool
    , isShanponWait :: Bool
    }

askWaitContext :: IO (WaitContext)
askWaitContext = do
    ryanmanWait <- askYesNo "Did the hand have an open wait? [y/n]: "
    shanponWait <-
        if ryanmanWait
            then return False
            else askYesNo "Did the hand have a dual pair wait? [y/n]: "
    return WaitContext{isRyanmanWait = ryanmanWait, isShanponWait = shanponWait}

data WindContext = WindContext
    {seatWind :: Wind, roundWind :: Wind}

askWindContext :: IO (WindContext)
askWindContext = do
    putStrLn "Input round and seat wind: "
    (Honour (Wind r) _) : (Honour (Wind s) _) : _ <- mkHand <$> getLine
    return WindContext{seatWind = s, roundWind = r}

data RiichiContext = RiichiContext
    {isRiichi :: Bool, isIppatsu :: Bool}

askRiichiContext :: IO (RiichiContext)
askRiichiContext = do
    riichi <- askYesNo "Riichi? [y/n]: "
    ippatsu <-
        if riichi
            then askYesNo "Ippatsu? [y/n]: "
            else return False
    return RiichiContext{isRiichi = riichi, isIppatsu = ippatsu}

data YakuContext = YakuContext
    -- , isTsumo :: Bool
    { isPinfu :: Bool
    , isTanyao :: Bool
    , isHaku :: Bool
    , isHatsu :: Bool
    , isChun :: Bool
    , isSeatWind :: Bool
    , isRoundWind :: Bool
    , isSanshokuDoujun :: Bool
    , isSanshokuDoukou :: Bool
    , isSanankou :: Bool
    , isToitoi :: Bool
    , isIttsuu :: Bool
    , isSankantsu :: Bool
    , isShousangen :: Bool
    , isChinitsu :: Bool
    , isHonitsu :: Bool
    , isRyanpeikou :: Bool
    , isIipeikou :: Bool
    , isJunchan :: Bool
    , isChanta :: Bool
    , isHonroutou :: Bool
    -- , isThirteenOrphans :: Bool
    -- , isChiitoitsu :: Bool
    }

data YakumanContext = YakumanContext
    { isSuuankou :: Bool
    , isSuukantsu :: Bool
    , isDaisangen :: Bool
    , isShousuushii :: Bool
    , isTsuuiisou :: Bool
    , isChinroutou :: Bool
    , isRyuuiisou :: Bool
    , isChuurenPoutou :: Bool
    , isDaisuushii :: Bool
    }

data Context = Context HandContext (Either YakuContext YakumanContext)

mkYakuContext :: Hand -> Maybe InterpretedHand -> HandContext -> YakuContext
mkYakuContext = undefined
