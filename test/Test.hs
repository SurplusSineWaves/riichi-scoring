import Test.Tasty
import Test.Tasty.HUnit

import Riichi.Tile

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "All tests"
        [ tileTests
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
        , testCase "Multi tile read test" $
            (readTileBlock "123p" @?= [Numeric Pin 1 0, Numeric Pin 2 0, Numeric Pin 3 0])
        ]
