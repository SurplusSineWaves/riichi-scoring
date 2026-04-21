{- |
Module      : Riichi.Scoring
Description : Functions for score calculation (yaku, han, fu)
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module Riichi.Scoring where

import ColourStrings
import Control.Monad (when)
import Control.Monad.Writer
import Data.Function ((&))
import Data.Map qualified as M
import Data.Monoid (Sum (..))
import Riichi.Context
import Riichi.Meld
import Riichi.Tile
import Riichi.Yaku

type YakumanCount = Sum Int
type Han = Sum Int

getYaku :: Hand -> Maybe InterpretedHand -> Bool -> Bool -> Bool -> Bool -> Wind -> Wind -> Bool -> (Either (Han, Han) YakumanCount, String)
getYaku hand (Just ih@(Pair _, melds)) riichi ippatsu tsumo ryanmanWait seatWind roundWind closedHand =
    -- The caller should ensure ih is not empty, as some of these yaku funcitons only look
    -- at the hand, and don't re-check if it has a valid interpretation.
    let
        -- Check Yakuman first.
        yakumanWriter :: Writer (YakumanCount, String) () = do
            when (suuankou ih) $ tell (1, toMagenta "\tYakuman: Four Concealed Triplets\n")
            when (suukantsu ih) $ tell (1, toMagenta "\tYakuman: Four Kans\n")
            when (daisangen ih) $ tell (1, toMagenta "\tYakuman: Big Four Dragons\n")
            when (shousuushii ih) $ tell (1, toMagenta "\tYakuman: Little Winds\n")
            when (tsuuiisou hand) $ tell (1, toMagenta "\tYakuman: All Honours\n")
            when (chinroutou hand) $ tell (1, toMagenta "\tYakuman: All Terminals\n")
            when (ryuuiisou hand) $ tell (1, toMagenta "\tYakuman: All Green\n")
            when (chuurenPoutou hand && closedHand) $ tell (1, toMagenta "\tYakuman: Nine Gates\n")
            when (daisuushii ih) $ tell (2, toMagenta "\tDouble Yakuman: Big Winds\n")
        (_, (yakumans, yakumanOutput)) = runWriter yakumanWriter

        hanWriter :: Writer (Han, Han, String) () = do
            when (riichi) $ tell (1, 0, toCyan "\t1 Han: Riichi\n")
            when (ippatsu) $ tell (1, 0, toCyan "\t1 Han: Ippatsu\n")
            when (tsumo && and (map meldIsClosed melds)) $ tell (1, 0, toCyan "\t1 Han: Fully concealed hand\n")
            when (pinfu ih seatWind roundWind ryanmanWait closedHand) $ tell (1, 0, toCyan "\t1 Han: Pinfu\n")
            when (tanyao hand) $ tell (1, 1, toCyan "\t1 Han: All simples\n")
            when (haku ih) $ tell (1, 1, toCyan "\t1 Han: Haku (White Dragon)\n")
            when (hatsu ih) $ tell (1, 1, toCyan "\t1 Han: Hatsu (Green Dragon)\n")
            when (chun ih) $ tell (1, 1, toCyan "\t1 Han: Chun (Red Dragon)\n")
            when (checkWind seatWind ih) $ tell (1, 1, toCyan "\t1 Han: Seat wind\n")
            when (checkWind roundWind ih) $ tell (1, 1, toCyan "\t1 Han: Round wind\n")
            when (sanshokuDoujun ih) $ tell (2, 1, toCyan "\t2 Han: Mixed triple sequence (-1 Han if open)\n")
            when (sanshokuDoukou ih) $ tell (2, 2, toCyan "\t2 Han: Triple triplets\n")
            when (sanankou ih) $ tell (2, 2, toCyan "\t2 Han: Three concealed triplets\n")
            let (fullFlush, halfFlush) = (chinitsu hand, honitsu hand)
            if fullFlush
                then
                    tell (6, 5, toCyan "\t6 Han: Full flush (-1 Han if open)\n")
                else
                    if halfFlush
                        then
                            tell (3, 2, toCyan "\t3 Han: Half flush (-1 Han if open)\n")
                        else
                            return ()
            when (toitoi ih) $ tell (2, 2, toCyan "\t2 Han: All triplets\n")
            when (ittsuu ih) $ tell (2, 1, toCyan "\t2 Han: Pure straight (-1 Han if open)\n")
            when (sankantsu ih) $ tell (2, 2, toCyan "\t2 Han: Three kans\n")
            when (shousangen ih) $ tell (2, 2, toCyan "\t2 Han: Little three dragons\n")
            let (twicePure, singlePure) = (ryanpeikou ih, iipeikou ih)
            if twicePure
                then
                    tell (3, 0, toCyan "\t3 Han: Twice pure double sequence (Closed only)\n")
                else
                    if singlePure
                        then
                            tell (1, 0, toCyan "\t1 Han: Pure double sequence (Closed only)\n")
                        else
                            return ()
            let (fullyOutside, halfOutside, terminalsHonours) = (junchan ih, chanta ih, honroutou hand)
            if fullyOutside
                then
                    tell (3, 2, toCyan "\t3 Han: Fully outside hand (-1 Han if open)\n")
                else
                    if terminalsHonours
                        then
                            tell (2, 2, toCyan "\t2 Han: All terminals and honours\n")
                        else
                            if halfOutside
                                then
                                    tell (2, 1, toCyan "\t2 Han: Half outside hand (-1 Han if open)\n")
                                else
                                    return ()
            when (dora > 0) $ tell (fromInteger dora, fromInteger dora, toCyan ("\t" ++ show dora ++ " Han: Dora\n"))
          where
            dora = hand & map getDora & sum

        (_, (hanClosed, hanOpen, output)) = runWriter hanWriter
     in
        if yakumans > 0
            then (Right yakumans, yakumanOutput)
            else
                if output == ""
                    then
                        (Left (0, 0), "\tNo explicit yaku found, riichi or menzen tsumo is required\n")
                    else
                        (Left (hanClosed, hanOpen), output)
getYaku hand Nothing riichi ippatsu tsumo _ _ _ _ =
    let
        -- Check Yakuman first.
        yakumanWriter :: Writer (YakumanCount, String) () = do
            when (thirteenOrphans hand) $ tell (1, toMagenta "\tYakuman: Thirteen Orphans (Double Yakuman if wait is 13 sided)\n")
            when (tsuuiisou hand) $ tell (1, toMagenta "\tYakuman: All Honours (+ seven pairs)\n")
        (_, (yakumans, yakumanOutput)) = runWriter yakumanWriter

        hanWriter :: Writer (Han, String) () = do
            when (riichi) $ tell (1, toCyan "\t1 Han: Riichi\n")
            when (ippatsu) $ tell (1, toCyan "\t1 Han: Ippatsu\n")
            when (tsumo) $ tell (1, toCyan "\t1 Han: Fully concealed hand\n")
            when (tanyao hand) $ tell (1, toCyan "\t1 Han: All simples\n")
            let (fullFlush, halfFlush) = (chinitsu hand, honitsu hand)
            if fullFlush
                then
                    tell (6, toCyan "\t6 Han: Full flush\n")
                else
                    if halfFlush
                        then
                            tell (3, toCyan "\t3 Han: Half flush\n")
                        else
                            return ()
            when (honroutou hand) $ tell (2, toCyan "\t2 Han: All terminals and honours\n")
            when (dora > 0) $ tell (fromInteger dora, toCyan ("\t" ++ show dora ++ " Han: Dora\n"))
          where
            dora = hand & map getDora & sum

        (_, (han, output)) = runWriter hanWriter
     in
        if yakumans > 0
            then (Right yakumans, yakumanOutput)
            else
                if chiitoitsu hand
                    then
                        (Left (han + 2, 0), toCyan "\t2 Han: Seven pairs\n" ++ output)
                    else
                        (Left (0, 0), "This hand is not valid\n")

getMeldFu :: Meld -> Fu
getMeldFu (Chi _ _ _ _) = 0
getMeldFu (Pon (Numeric _ v _) True) = if v `elem` [1, 9] then 4 else 2
getMeldFu (Pon (Honour _ _) True) = 4
getMeldFu (Pon (Numeric _ v _) False) = if v `elem` [1, 9] then 8 else 4
getMeldFu (Pon (Honour _ _) False) = 8
getMeldFu (Kan (Numeric _ v _) True) = if v `elem` [1, 9] then 16 else 8
getMeldFu (Kan (Honour _ _) True) = 16
getMeldFu (Kan (Numeric _ v _) False) = if v `elem` [1, 9] then 32 else 16
getMeldFu (Kan (Honour _ _) False) = 32

-- Get fu for a standard hand. Seven pairs and thirteen orphans, as ever, are handled separately
type Fu = Int
getFu :: InterpretedHand -> Wind -> Wind -> Bool -> Bool -> Bool -> Fu
getFu (Pair tile, melds) seatWind roundWind goodWait tsumo closedHand =
    -- Perhaps a writer monad over the sum int monoid would be more elegant here but I think this more
    -- descriptive method is fine too.
    let meldsFu = melds & map getMeldFu & sum
        waitFu = if goodWait then 2 else 0
        yakuhaiFu =
            (if (tile & isDragon) then 2 else 0)
                + (if (tile == (Honour (Wind roundWind) 0)) then 2 else 0)
                + (if (tile == (Honour (Wind seatWind) 0)) then 2 else 0)
        ronClosedFu = if (not tsumo) && closedHand then 10 else 0
        tsumoFu = if tsumo then 2 else 0
     in roundUp (20 + meldsFu + waitFu + yakuhaiFu + ronClosedFu + tsumoFu)
  where
    roundUp n = last ([120, 110 .. 10] & filter (>= n))

getScore :: Han -> Fu -> Bool -> Bool -> Integer
getScore han fu dealer tsumo =
    if dealer
        then case han of
            _
                | han >= 13 -> 32000
                | han >= 5 -> case M.lookup han manganToSanbaimanTableDealer of
                    Just score -> score
                    Nothing -> 0
                | otherwise ->
                    if tsumo
                        then case M.lookup (han, fu) scoreTableTsumoDealer of
                            Just score -> score
                            Nothing -> 0
                        else case M.lookup (han, fu) scoreTableRonDealer of
                            Just score -> score
                            Nothing -> 0
        else case han of
            _
                | han >= 13 -> 24000
                | han >= 5 -> case M.lookup han manganToSanbaimanTableNonDealer of
                    Just score -> score
                    Nothing -> 0
                | otherwise ->
                    if tsumo
                        then case M.lookup (han, fu) scoreTableTsumoNonDealer of
                            Just score -> score
                            Nothing -> 0
                        else case M.lookup (han, fu) scoreTableRonNonDealer of
                            Just score -> score
                            Nothing -> 0

formYakuString :: YakuContext -> String
formYakuString yakuContext@YakuContext{yakuHandContext = handContext@HandContext{riichi = riichiContext, dora}} =
    let hanWriter :: Writer String () = do
            when (isRiichi riichiContext) $ tell $ toCyan "\t1 Han: Riichi\n"
            when (isIppatsu riichiContext) $ tell $ toCyan "\t1 Han: Ippatsu\n"
            when (isMenzenTsumo yakuContext) $ tell $ toCyan "\t1 Han: Fully concealed hand\n"
            when (isChiitoitsu yakuContext) $ tell $ toCyan "\t2 Han: Seven pairs\n"
            when (isPinfu yakuContext) $ tell $ toCyan "\t1 Han: Pinfu\n"
            when (isTanyao yakuContext) $ tell $ toCyan "\t1 Han: All simples\n"
            when (isHaku yakuContext) $ tell $ toCyan "\t1 Han: Haku (White Dragon)\n"
            when (isHatsu yakuContext) $ tell $ toCyan "\t1 Han: Hatsu (Green Dragon)\n"
            when (isChun yakuContext) $ tell $ toCyan "\t1 Han: Chun (Red Dragon)\n"
            when (isSeatWind yakuContext) $ tell $ toCyan "\t1 Han: Seat wind\n"
            when (isRoundWind yakuContext) $ tell $ toCyan "\t1 Han: Round wind\n"
            when (isSanshokuDoujun yakuContext) $ tell $ toCyan "\t2 Han: Mixed triple sequence (-1 Han if open)\n"
            when (isSanshokuDoukou yakuContext) $ tell $ toCyan "\t2 Han: Triple triplets\n"
            when (isSanankou yakuContext) $ tell $ toCyan "\t2 Han: Three concealed triplets\n"
            when (isChinitsu yakuContext) $ tell $ toCyan "\t6 Han: Full flush (-1 Han if open)\n"
            when (isHonitsu yakuContext) $ tell $ toCyan "\t3 Han: Half flush (-1 Han if open)\n"
            when (isToitoi yakuContext) $ tell $ toCyan "\t2 Han: All triplets\n"
            when (isIttsuu yakuContext) $ tell $ toCyan "\t2 Han: Pure straight (-1 Han if open)\n"
            when (isSankantsu yakuContext) $ tell $ toCyan "\t2 Han: Three kans\n"
            when (isShousangen yakuContext) $ tell $ toCyan "\t2 Han: Little three dragons\n"
            when (isRyanpeikou yakuContext) $ tell $ toCyan "\t3 Han: Twice pure double sequence (Closed only)\n"
            when (isIipeikou yakuContext) $ tell $ toCyan "\t1 Han: Pure double sequence (Closed only)\n"
            when (isJunchan yakuContext) $ tell $ toCyan "\t3 Han: Fully outside hand (-1 Han if open)\n"
            when (isHonroutou yakuContext) $ tell $ toCyan "\t2 Han: All terminals and honours\n"
            when (isChanta yakuContext) $ tell $ toCyan "\t2 Han: Half outside hand (-1 Han if open)\n"
            when (dora > 0) $ tell $ toCyan ("\t" ++ show dora ++ " Han: Dora\n")
        (_, string) = runWriter hanWriter
     in string

getYakuHan :: YakuContext -> Han
getYakuHan yakuContext@YakuContext{yakuHandContext = handContext@HandContext{riichi = riichiContext, dora}} =
    let
        closedBonus = if (isClosed handContext == True) then 1 else 0
        hanWriter :: Writer Han () = do
            when (isRiichi riichiContext) $ tell $ 1
            when (isIppatsu riichiContext) $ tell $ 1
            when (isMenzenTsumo yakuContext) $ tell $ 1
            when (isChiitoitsu yakuContext) $ tell $ 2
            when (isPinfu yakuContext) $ tell $ 1
            when (isTanyao yakuContext) $ tell $ 1
            when (isHaku yakuContext) $ tell $ 1
            when (isHatsu yakuContext) $ tell $ 1
            when (isChun yakuContext) $ tell $ 1
            when (isSeatWind yakuContext) $ tell $ 1
            when (isRoundWind yakuContext) $ tell $ 1
            when (isSanshokuDoujun yakuContext) $ tell $ 1 + closedBonus
            when (isSanshokuDoukou yakuContext) $ tell $ 2
            when (isSanankou yakuContext) $ tell $ 2
            when (isChinitsu yakuContext) $ tell $ 5 + closedBonus
            when (isHonitsu yakuContext) $ tell $ 2 + closedBonus
            when (isToitoi yakuContext) $ tell $ 2
            when (isIttsuu yakuContext) $ tell $ 1 + closedBonus
            when (isSankantsu yakuContext) $ tell $ 2
            when (isShousangen yakuContext) $ tell $ 2
            when (isRyanpeikou yakuContext) $ tell $ 3 * closedBonus
            when (isIipeikou yakuContext) $ tell $ closedBonus
            when (isJunchan yakuContext) $ tell $ 2 + closedBonus
            when (isHonroutou yakuContext) $ tell $ 2
            when (isChanta yakuContext) $ tell $ 1 + closedBonus
            tell $ fromInteger dora
        (_, han) = runWriter hanWriter
     in
        han

formYakumanString :: YakumanContext -> String
formYakumanString yakumanContext =
    let
        yakumanWriter :: Writer String () = do
            when (isSuuankou yakumanContext) $ tell $ toMagenta "\tYakuman: Four Concealed Triplets\n"
            when (isSuukantsu yakumanContext) $ tell $ toMagenta "\tYakuman: Four Kans\n"
            when (isDaisangen yakumanContext) $ tell $ toMagenta "\tYakuman: Big Four Dragons\n"
            when (isShousuushii yakumanContext) $ tell $ toMagenta "\tYakuman: Little Winds\n"
            when (isTsuuiisou yakumanContext) $ tell $ toMagenta "\tYakuman: All Honours\n"
            when (isChinroutou yakumanContext) $ tell $ toMagenta "\tYakuman: All Terminals\n"
            when (isRyuuiisou yakumanContext) $ tell $ toMagenta "\tYakuman: All Green\n"
            when (isChuurenPoutou yakumanContext) $ tell $ toMagenta "\tYakuman: Nine Gates\n"
            when (isDaisuushii yakumanContext) $ tell $ toMagenta "\tDouble Yakuman: Big Winds\n"
            when (isKokushiMusou yakumanContext) $ tell $ toMagenta "\tYakuman: Thirteen Orphans\n"
        (_, string) = runWriter yakumanWriter
     in
        string
getYakumanCount :: YakumanContext -> Int
getYakumanCount yakumanContext =
    let
        yakumanWriter :: Writer (Sum Int) () = do
            when (isSuuankou yakumanContext) $ tell 1
            when (isSuukantsu yakumanContext) $ tell 1
            when (isDaisangen yakumanContext) $ tell 1
            when (isShousuushii yakumanContext) $ tell 1
            when (isTsuuiisou yakumanContext) $ tell 1
            when (isChinroutou yakumanContext) $ tell 1
            when (isRyuuiisou yakumanContext) $ tell 1
            when (isChuurenPoutou yakumanContext) $ tell 1
            when (isDaisuushii yakumanContext) $ tell 2
            when (isKokushiMusou yakumanContext) $ tell 1
        (_, yakumans) = runWriter yakumanWriter
     in
        getSum yakumans

formContextString :: Context -> String
formContextString (Context _ _ (Left yakuContext)) = formYakuString yakuContext
formContextString (Context _ _ (Right yakumanContext)) = formYakumanString yakumanContext

getContextHanOrYakumans :: Context -> Either Han Int
getContextHanOrYakumans (Context _ _ (Left yakuContext)) = Left $ getYakuHan yakuContext
getContextHanOrYakumans (Context _ _ (Right yakumanContext)) = Right $ getYakumanCount yakumanContext

getContextHansOrYakumans :: Context -> Either (Han, Han) Int
getContextHansOrYakumans (Context _ _ (Left yakuContext@YakuContext{yakuHandContext = handContext})) =
    Left $
        ( getYakuHan yakuContext{yakuHandContext = closeHandContext handContext}
        , getYakuHan yakuContext{yakuHandContext = openHandContext handContext}
        )
getContextHansOrYakumans (Context _ _ (Right yakumanContext)) = Right $ getYakumanCount yakumanContext

_getFu :: InterpretedHand -> HandContext -> Fu
_getFu (Pair tile, melds) c =
    let
        sw = seatWind $ wind c
        rw = roundWind $ wind c
        goodWait = not $ (isRyanmanWait $ wait c) || (isShanponWait $ wait c)
        tsumo = isTsumo c
        closure = isClosed c
        meldsFu = melds & map getMeldFu & sum
        waitFu = if goodWait then 2 else 0
        yakuhaiFu =
            (if (tile & isDragon) then 2 else 0)
                + (if (tile == (Honour (Wind rw) 0)) then 2 else 0)
                + (if (tile == (Honour (Wind sw) 0)) then 2 else 0)
        ronClosedFu = if (not tsumo) && closure then 10 else 0
        tsumoFu = if tsumo then 2 else 0
     in
        roundUp (20 + meldsFu + waitFu + yakuhaiFu + ronClosedFu + tsumoFu)
  where
    roundUp n = last ([120, 110 .. 10] & filter (>= n))

-- Partial function!
getContextFu :: Context -> Fu
getContextFu (Context (Just ih) handContext _) = _getFu ih handContext
getContextFu (Context Nothing handContext _) = 25

scoreTableTsumoDealer :: M.Map (Han, Fu) Integer
scoreTableTsumoDealer =
    M.fromList
        [ ((1, 20), 1200)
        , ((1, 30), 1500)
        , ((1, 40), 2100)
        , ((1, 50), 2400)
        , ((1, 60), 3000)
        , ((1, 70), 3600)
        , ((1, 80), 3900)
        , ((1, 90), 4500)
        , ((1, 100), 4800)
        , ((1, 110), 5400)
        , ((2, 20), 2100)
        , ((2, 30), 3000)
        , ((2, 40), 3900)
        , ((2, 50), 4800)
        , ((2, 60), 6000)
        , ((2, 70), 6900)
        , ((2, 80), 7800)
        , ((2, 90), 8700)
        , ((2, 100), 9600)
        , ((2, 110), 10800)
        , ((3, 20), 3900)
        , ((3, 25), 4800)
        , ((3, 30), 6000)
        , ((3, 40), 7800)
        , ((3, 50), 9600)
        , ((3, 60), 11700)
        , ((3, 70), 12000)
        , ((3, 80), 12000)
        , ((3, 90), 12000)
        , ((3, 100), 12000)
        , ((3, 110), 12000)
        , ((4, 20), 7800)
        , ((4, 25), 9600)
        , ((4, 30), 11700)
        , ((4, 40), 12000)
        , ((4, 50), 12000)
        , ((4, 60), 12000)
        , ((4, 70), 12000)
        , ((4, 80), 12000)
        , ((4, 90), 12000)
        , ((4, 100), 12000)
        , ((4, 110), 12000)
        ]

scoreTableRonDealer :: M.Map (Han, Fu) Integer
scoreTableRonDealer =
    M.fromList
        [ ((1, 30), 1500)
        , ((1, 40), 2000)
        , ((1, 50), 2400)
        , ((1, 60), 2900)
        , ((1, 70), 3400)
        , ((1, 80), 3900)
        , ((1, 90), 4400)
        , ((1, 100), 4800)
        , ((1, 110), 5300)
        , ((2, 25), 2400)
        , ((2, 30), 2900)
        , ((2, 40), 3900)
        , ((2, 50), 4800)
        , ((2, 60), 5800)
        , ((2, 70), 6800)
        , ((2, 80), 7700)
        , ((2, 90), 8700)
        , ((2, 100), 9600)
        , ((2, 110), 10600)
        , ((3, 25), 4800)
        , ((3, 30), 5800)
        , ((3, 40), 7700)
        , ((3, 50), 9600)
        , ((3, 60), 11600)
        , ((3, 70), 12000)
        , ((3, 80), 12000)
        , ((3, 90), 12000)
        , ((3, 100), 12000)
        , ((3, 110), 12000)
        , ((4, 25), 9600)
        , ((4, 30), 11600)
        , ((4, 40), 12000)
        , ((4, 50), 12000)
        , ((4, 60), 12000)
        , ((4, 70), 12000)
        , ((4, 80), 12000)
        , ((4, 90), 12000)
        , ((4, 100), 12000)
        , ((4, 110), 12000)
        ]

scoreTableTsumoNonDealer :: M.Map (Han, Fu) Integer
scoreTableTsumoNonDealer =
    M.fromList
        [ ((1, 20), 800)
        , ((1, 30), 1100)
        , ((1, 40), 1500)
        , ((1, 50), 1600)
        , ((1, 60), 2000)
        , ((1, 70), 2400)
        , ((1, 80), 2700)
        , ((1, 90), 3100)
        , ((1, 100), 3200)
        , ((1, 110), 3600)
        , ((2, 20), 1500)
        , ((2, 30), 2000)
        , ((2, 40), 2700)
        , ((2, 50), 3200)
        , ((2, 60), 4000)
        , ((2, 70), 4700)
        , ((2, 80), 5200)
        , ((2, 90), 5900)
        , ((2, 100), 6400)
        , ((2, 110), 7200)
        , ((3, 20), 2700)
        , ((3, 25), 3200)
        , ((3, 30), 4000)
        , ((3, 40), 5200)
        , ((3, 50), 6400)
        , ((3, 60), 7900)
        , ((3, 70), 8000)
        , ((3, 80), 8000)
        , ((3, 90), 8000)
        , ((3, 100), 8000)
        , ((3, 110), 8000)
        , ((4, 20), 5200)
        , ((4, 25), 6400)
        , ((4, 30), 7900)
        , ((4, 40), 8000)
        , ((4, 50), 8000)
        , ((4, 60), 8000)
        , ((4, 70), 8000)
        , ((4, 80), 8000)
        , ((4, 90), 8000)
        , ((4, 100), 8000)
        , ((4, 110), 8000)
        ]

scoreTableRonNonDealer :: M.Map (Han, Fu) Integer
scoreTableRonNonDealer =
    M.fromList
        [ ((1, 30), 1000)
        , ((1, 40), 1300)
        , ((1, 50), 1600)
        , ((1, 60), 2000)
        , ((1, 70), 2300)
        , ((1, 80), 2600)
        , ((1, 90), 2900)
        , ((1, 100), 3200)
        , ((1, 110), 3600)
        , ((2, 25), 1600)
        , ((2, 30), 2000)
        , ((2, 40), 2600)
        , ((2, 50), 3200)
        , ((2, 60), 3900)
        , ((2, 70), 4500)
        , ((2, 80), 5200)
        , ((2, 90), 5800)
        , ((2, 100), 6400)
        , ((2, 110), 7100)
        , ((3, 25), 3200)
        , ((3, 30), 3900)
        , ((3, 40), 5200)
        , ((3, 50), 6400)
        , ((3, 60), 7700)
        , ((3, 70), 8000)
        , ((3, 80), 8000)
        , ((3, 90), 8000)
        , ((3, 100), 8000)
        , ((3, 110), 8000)
        , ((4, 25), 6400)
        , ((4, 30), 7700)
        , ((4, 40), 8000)
        , ((4, 50), 8000)
        , ((4, 60), 8000)
        , ((4, 70), 8000)
        , ((4, 80), 8000)
        , ((4, 90), 8000)
        , ((4, 100), 8000)
        , ((4, 110), 8000)
        ]

manganToSanbaimanTableDealer :: M.Map Han Integer
manganToSanbaimanTableDealer =
    M.fromList
        [ (5, 12000)
        , (6, 18000)
        , (7, 18000)
        , (8, 24000)
        , (9, 24000)
        , (10, 24000)
        , (11, 36000)
        , (12, 36000)
        ]

manganToSanbaimanTableNonDealer :: M.Map Han Integer
manganToSanbaimanTableNonDealer =
    M.fromList
        [ (5, 8000)
        , (6, 12000)
        , (7, 12000)
        , (8, 16000)
        , (9, 16000)
        , (10, 16000)
        , (11, 24000)
        , (12, 24000)
        ]

hanToHandName :: Han -> String
hanToHandName 5 = "Mangan"
hanToHandName 6 = "Haneman"
hanToHandName 7 = "Haneman"
hanToHandName 8 = "Baiman"
hanToHandName 9 = "Baiman"
hanToHandName 10 = "Baiman"
hanToHandName 11 = "Sanbaiman"
hanToHandName 12 = "Sanbaiman"
hanToHandName n = if n >= 13 then "Counted Yakuman" else ""
