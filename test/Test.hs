import Test.Tasty
import Test.Tasty.HUnit

import Data.Maybe
import Data.Set qualified as Set
import Riichi.Context
import Riichi.Meld
import Riichi.Scoring
import Riichi.Tile

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All tests"
        [ tileTests
        , meldsTests
        , scoringTests
        , showTests
        ]

tileTests :: TestTree
tileTests =
    testGroup
        "Tile test group"
        [ testCase "Tile equality ignores dora" $
            (Numeric Pin 7 0 == Numeric Pin 7 1)
                && (Honour (Wind North) 1 == Honour (Wind North) 0) @?= True
        , testCase "Single tile read test" $
            ((read "0m" :: Tile) == (read "5m" :: Tile))
                && (read "r" == Honour (Dragon Red) 1) @?= True
        , testCase "Multi tile read test 1" $
            (readTileBlock "123p" @?= [Numeric Pin 1 0, Numeric Pin 2 0, Numeric Pin 3 0])
        , testCase "Multi tile read test 2" $
            (readTileBlock "rwN" @?= [Honour (Dragon Red) 0, Honour (Dragon White) 0, Honour (Wind North) 0])
        ]

meldsTests :: TestTree
meldsTests =
    testGroup
        "Melds test group"
        [ testCase "Form melds test 1" $
            let
                hand = mkHand "13p 3333s 2p N 65m rrr 7m N"
                (pair, melds) = head $ interpretHand hand
                expectedPair = Pair (Honour (Wind North) 4)
                expextedMelds =
                    Set.fromList
                        [ Kan (Numeric Sou 3 0) False
                        , Chi (Numeric Pin 1 0) (Numeric Pin 2 0) (Numeric Pin 3 1) False
                        , Chi (Numeric Man 7 0) (Numeric Man 5 2) (Numeric Man 6 0) True
                        , Pon (Honour (Dragon Red) 1) False
                        ]
             in
                (Set.fromList melds == expextedMelds) && (pair == expectedPair) @?= True
        , testCase "Form melds test 2" $
            let
                hand = mkHand "222m 333m 444m 555m 77p"
                ihs = interpretHand hand
             in
                (length ihs) @?= 3
        ]

scoringTests :: TestTree
scoringTests =
    testGroup
        "Scoring test group"
        [ testCase "Scoring test 1" $
            testHanHandScore "567m 44s N EgEgEg NN" "6p" False False False False False North East False @?= (5, 50, 8000)
        , testCase "Scoring test 2" $
            testHanHandScore "345789m 78999s ggg" "9s" False False True False True West East False @?= (4, 40, 12000)
        , testCase "Scoring test 3" $
            testHanHandScore "345789m 78999s ggg" "9s" False False False False True West East False @?= (4, 30, 11600)
        , testCase "Scoring test 4" $
            testHanHandScore "567p 33557788993s" "" True False False True False South South False @?= (2, 40, 2600)
        , testCase "Scoring test 5" $
            testHanHandScore "22221345663p NNN" "r" False False False False False North South False @?= (3, 40, 5200)
        , testCase "Scoring test 6" $
            testHanHandScore "234m 7p 567s 34p 66p 66p 57p" "5s w" False False False False True North South False @?= (2, 40, 3900)
        , testCase "Scoring test 7" $
            testHanHandScore "22p 55m 33s 88p rr NN EE" "N" False False False False True North South True @?= (4, 25, 9600)
        ]

showTests :: TestTree
showTests =
    testGroup
        "Show test group"
        [ testCase "Tile show test 1" $ (show $ (read $ "1p" :: Tile)) @?= "1p"
        , testCase "Tile show test 2" $ (show $ (read $ "2m" :: Tile)) @?= "2m"
        , testCase "Tile show test 3" $ (show $ (read $ "3s" :: Tile)) @?= "3s"
        , testCase "Tile show test 4" $ (show $ (read $ "r" :: Tile)) @?= "r"
        , testCase "Tile show test 5" $ (show $ (read $ "g" :: Tile)) @?= "g"
        , testCase "Tile show test 6" $ (show $ (read $ "w" :: Tile)) @?= "w"
        , testCase "Tile show test 7" $ (show $ (read $ "N" :: Tile)) @?= "N"
        , testCase "Tile show test 8" $ (show $ (read $ "S" :: Tile)) @?= "S"
        , testCase "Tile show test 9" $ (show $ (read $ "E" :: Tile)) @?= "E"
        , testCase "Tile show test 10" $ (show $ (read $ "W" :: Tile)) @?= "W"
        , testCase "Tile show test 11" $ (show $ (read $ "0p" :: Tile)) @?= "5p*"
        , testCase "Hand show test" $
            (showInterpretedHand $ (head $ interpretHand $ (mkHand "123p 456m 789s NNN rr")))
                @?= "Pair r, Closed chi: 456 Man, Closed chi: 123 Pin, Closed chi: 789 Sou, Closed pon: NNN"
        ]

testHanHandScore handString doraString riichi ippatsu tsumo closed dealer sw rw sevenPairs =
    let
        hand = mkHand handString
        dora = mkHand doraString
        hand' = addDora dora hand
        handContext = getMinimalHandContext hand' sevenPairs
        handContext' =
            handContext
                { riichi =
                    RiichiContext
                        { isRiichi = riichi
                        , isIppatsu = ippatsu
                        }
                , isTsumo = tsumo
                , isClosed = closed
                , wind =
                    WindContext
                        { seatWind = sw
                        , roundWind = rw
                        }
                }
        ih = listToMaybe $ interpretHand hand'
        yakuContext = mkYakuContext hand' ih handContext'
        context = Context ih handContext' (Left yakuContext)
        (Left han) = getContextHanOrYakumans context
        fu = getContextFu context
        score = getScore han fu dealer tsumo
     in
        (han, fu, score)
