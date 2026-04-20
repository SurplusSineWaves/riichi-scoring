module Riichi.Context where

import Data.Function ((&))
import Riichi.Meld
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
    { isClosed :: Bool
    , isTsumo :: Bool
    , riichi :: RiichiContext
    , wait :: WaitContext
    , wind :: WindContext
    , isSevenPairs :: Bool
    , isThirteenOrphans :: Bool
    , dora :: Integer
    }

getMinimalHandContext :: Hand -> Bool -> HandContext
getMinimalHandContext hand sp =
    let
        sevenPairs = sp
        orphans = thirteenOrphans hand
     in
        HandContext
            { isClosed = False
            , isTsumo = False
            , riichi = RiichiContext{isRiichi = False, isIppatsu = False}
            , wait = WaitContext{isRyanmanWait = False, isShanponWait = False}
            , wind = WindContext{seatWind = East, roundWind = East}
            , isSevenPairs = sevenPairs
            , isThirteenOrphans = orphans
            , dora = hand & map getDora & sum
            }

addRiichiContext :: HandContext -> IO HandContext
addRiichiContext handContext = do
    riichiContext <- askRiichiContext
    return handContext{riichi = riichiContext}

addWaitContext :: HandContext -> IO HandContext
addWaitContext handContext = do
    waitContext <- askWaitContext
    return handContext{wait = waitContext}

addWindContext :: HandContext -> IO HandContext
addWindContext handContext = do
    windContext <- askWindContext
    return handContext{wind = windContext}

addTsumoContext :: HandContext -> IO HandContext
addTsumoContext handContext = do
    tsumo <- askYesNo "Tsumo? [y/n]:"
    return handContext{isClosed = tsumo}

-- The context must already know riichi and tsumo values for this to work
addClosedContext :: Maybe InterpretedHand -> HandContext -> IO (Maybe InterpretedHand, HandContext)
addClosedContext (Just (tile, melds)) handContext = do
    let HandContext{isTsumo = t, riichi = RiichiContext{isRiichi = r}} = handContext
    melds' <- case (r, t) of
        (True, True) -> return melds
        (True, False) -> getRonMeld melds
        (False, _) -> getOpenMelds melds
    let numOpen = melds' & filter isOpen & length
    closedHand <- case numOpen of
        0 -> return True
        _
            | numOpen > 1 -> return False
            | otherwise -> case (r, t) of
                (True, _) -> return True
                (False, True) -> return False -- Already know there is an open meld. Now we know it wasn't opened by Ron.
                (False, False) -> askYesNo "Damaten? [y/n]:"
    return (Just (tile, melds'), handContext{isClosed = closedHand})
addClosedContext Nothing handContext = return (Nothing, handContext{isClosed = True})

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
    , isMenzenTsumo :: Bool
    , yakuHandContext :: HandContext
    , -- , isThirteenOrphans :: Bool
      isChiitoitsu :: Bool
    }

mkYakuContext :: Hand -> Maybe InterpretedHand -> HandContext -> YakuContext
mkYakuContext hand (Just ih) handContext =
    let
        HandContext
            { wind = WindContext{seatWind = sw, roundWind = rw}
            , wait = WaitContext{isRyanmanWait = isRyanman}
            , isClosed = closure
            , isTsumo = tsumo
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
              isRyanpeikou = twicePure && closure
            , isIipeikou = singlePure && (not twicePure) && closure
            , isJunchan = fullyOutside
            , isChanta = halfOutside && (not fullyOutside) && (not terminalsHonours)
            , isHonroutou = terminalsHonours && (not fullyOutside)
            , yakuHandContext = handContext
            , isMenzenTsumo = tsumo && closure
            , isChiitoitsu = False
            }
-- Seven pairs case
mkYakuContext hand Nothing handContext@HandContext{isTsumo = tsumo} =
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
            , yakuHandContext = handContext
            , isMenzenTsumo = tsumo
            , isChiitoitsu = chiitoitsu hand
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

mkYakumanContext :: Hand -> Maybe InterpretedHand -> Maybe YakumanContext
mkYakumanContext hand (Just ih) =
    let
        isSuuaa = suuankou ih
        isSuuka = suukantsu ih
        isDaisa = daisangen ih
        isShous = shousuushii ih
        isTsuui = tsuuiisou hand
        isChinr = chinroutou hand
        isRyuui = ryuuiisou hand
        isChuur = chuurenPoutou hand
        isDaisu = daisuushii ih
     in
        if or [isSuuaa, isSuuka, isDaisa, isShous, isTsuui, isChinr, isRyuui, isChuur, isDaisu]
            then
                Just
                    YakumanContext
                        { isSuuankou = isSuuaa
                        , isSuukantsu = isSuuka
                        , isDaisangen = isDaisa
                        , isShousuushii = isShous
                        , isTsuuiisou = isTsuui
                        , isChinroutou = isChinr
                        , isRyuuiisou = isRyuui
                        , isChuurenPoutou = isChuur
                        , isDaisuushii = isDaisu
                        }
            else
                Nothing
mkYakumanContext hand Nothing =
    let
        isTsuui = tsuuiisou hand
        isChinr = chinroutou hand
        isRyuui = ryuuiisou hand
        isChuur = chuurenPoutou hand
     in
        if or [isTsuui, isChinr, isRyuui, isChuur]
            then
                Just
                    YakumanContext
                        { isSuuankou = False
                        , isSuukantsu = False
                        , isDaisangen = False
                        , isShousuushii = False
                        , isTsuuiisou = isTsuui
                        , isChinroutou = isChinr
                        , isRyuuiisou = isRyuui
                        , isChuurenPoutou = isChuur
                        , isDaisuushii = False
                        }
            else
                Nothing
data Context = Context (Maybe InterpretedHand) HandContext (Either YakuContext YakumanContext)

mkContext :: Hand -> IO Context
mkContext hand = do
    sevenPairs <-
        if chiitoitsu hand
            then askYesNo "Seven pairs? [y/n]: "
            else return False
    let handContext = getMinimalHandContext hand sevenPairs
    maybeIh <-
        if sevenPairs
            then
                return Nothing
            else do
                let ihs = interpretHand hand
                ih <-
                    if length ihs > 1
                        then do
                            putStrLn "Select hand interpretation: "
                            sequence_ $ [("[" ++ show n ++ "]: " ++ (ih & showInterpretedHand)) & putStrLn | (n :: Integer, ih) <- zip [0 ..] ihs]
                            n <- read <$> getLine :: IO Int
                            return (ihs !! n)
                        else do
                            putStrLn "Found one way to interpret this hand: "
                            let ih = head ihs
                            putStrLn (showInterpretedHand ih)
                            return ih
                return $ Just ih
    let maybeYakumanContext = mkYakumanContext hand maybeIh
    case maybeYakumanContext of
        Nothing -> do
            handContext' <- pure handContext >>= addWindContext >>= addRiichiContext >>= addTsumoContext >>= addWaitContext
            (maybeIh', handContext'') <- addClosedContext maybeIh handContext'
            let yakuContext = mkYakuContext hand maybeIh' handContext''
            return $ Context maybeIh' handContext'' (Left yakuContext)
        Just yakumanContext -> do
            return $ Context maybeIh handContext (Right yakumanContext)
