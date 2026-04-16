module Riichi.Context where

import Data.Function ((&))
import Riichi.Meld (Hand, InterpretedHand, Pair (Pair), getDora, isOpen, mkHand)
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
    , dora :: Integer
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
            , dora = hand & map getDora & sum
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

addTsumoContext :: HandContext -> IO HandContext
addTsumoContext handContext = do
    tsumo <- askYesNo "Tsumo? [y/n]:"
    return handContext{isClosed = Just tsumo}

-- The context must already know riichi and tsumo values for this to work
addClosedContext :: Hand -> Maybe InterpretedHand -> HandContext -> IO HandContext
addClosedContext hand (Just (_, melds)) handContext = do
    let HandContext{isTsumo = Just t, riichi = Just RiichiContext{isRiichi = r}} = handContext
    let numOpen = melds & filter isOpen & length
    closedHand <- case numOpen of
        0 -> return True
        _
            | numOpen > 1 -> return False
            | otherwise -> case (r, t) of
                (True, _) -> return True
                (False, True) -> return False -- Already know there is an open meld. Now we know it wasn't opened by Ron.
                (False, False) -> askYesNo "Damaten? [y/n]:"
    return handContext{isClosed = Just closedHand}
addClosedContext _ Nothing handContext = return handContext{isClosed = Just True}

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

mkYakuContext :: Hand -> Maybe InterpretedHand -> HandContext -> YakuContext
mkYakuContext hand (Just ih) handContext =
    let
        HandContext
            { wind = Just WindContext{seatWind = sw, roundWind = rw}
            , wait = Just WaitContext{isRyanmanWait = isRyanman}
            , isClosed = Just closure
            } = handContext
        (fullFlush, halfFlush) = (chinitsu hand, honitsu hand)
        (twicePure, singlePure) = (ryanpeikou ih, iipeikou ih)
        (fullyOutside, halfOutside, terminalsHonours) = (junchan ih, chanta ih, honroutou hand)
     in
        YakuContext
            { isPinfu = pinfu ih sw rw isRyanman closure
            , isTanyao = tanyao hand
            , isHaku = haku ih
            , isHatsu = hatsu ih
            , isChun = chun ih
            , isSeatWind = checkWind sw ih
            , isRoundWind = checkWind rw ih
            , isSanshokuDoujun = sanshokuDoujun ih
            , isSanshokuDoukou = sanshokuDoukou ih
            , isSanankou = sanankou ih
            , isToitoi = toitoi ih
            , isIttsuu = ittsuu ih
            , isSankantsu = sankantsu ih
            , isShousangen = shousangen ih
            , isChinitsu = fullFlush
            , isHonitsu = halfFlush && (not fullFlush)
            , -- Should these check for closed, or do we want to include them anyway?
              isRyanpeikou = twicePure
            , isIipeikou = singlePure && (not twicePure)
            , isJunchan = fullyOutside
            , isChanta = halfOutside && (not fullyOutside) && (not terminalsHonours)
            , isHonroutou = terminalsHonours && (not fullyOutside)
            }
-- Seven pairs case
mkYakuContext hand Nothing handContext =
    let
        (fullFlush, halfFlush) = (chinitsu hand, honitsu hand)
        terminalsHonours = honroutou hand
     in
        YakuContext
            { isPinfu = False
            , isTanyao = tanyao hand
            , isHaku = False
            , isHatsu = False
            , isChun = False
            , isSeatWind = False
            , isRoundWind = False
            , isSanshokuDoujun = False
            , isSanshokuDoukou = False
            , isSanankou = False
            , isToitoi = False
            , isIttsuu = False
            , isSankantsu = False
            , isShousangen = False
            , isChinitsu = fullFlush
            , isHonitsu = halfFlush && (not fullFlush)
            , -- Should these check for closed, or do we want to include them anyway?
              isRyanpeikou = False
            , isIipeikou = False
            , isJunchan = False
            , isChanta = False
            , isHonroutou = terminalsHonours
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

mkYakumanContext :: Hand -> Maybe InterpretedHand -> YakumanContext
mkYakumanContext hand (Just ih) =
    YakumanContext
        { isSuuankou = suuankou ih
        , isSuukantsu = suukantsu ih
        , isDaisangen = daisangen ih
        , isShousuushii = shousuushii ih
        , isTsuuiisou = tsuuiisou hand
        , isChinroutou = chinroutou hand
        , isRyuuiisou = ryuuiisou hand
        , isChuurenPoutou = chuurenPoutou hand
        , isDaisuushii = daisuushii ih
        }
mkYakumanContext hand Nothing = undefined

data Context = Context HandContext (Either YakuContext YakumanContext)
