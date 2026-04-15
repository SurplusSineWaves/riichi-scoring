module Riichi.ColourStrings where

toRed :: String -> String
toRed s = "\o33[31m" ++ s ++ "\o33[0m"

toBlue :: String -> String
toBlue s = "\o33[34m" ++ s ++ "\o33[0m"

toMagenta :: String -> String
toMagenta s = "\o33[35m" ++ s ++ "\o33[0m"

toCyan :: String -> String
toCyan s = "\o33[36m" ++ s ++ "\o33[0m"

toGreen :: String -> String
toGreen s = "\o33[32m" ++ s ++ "\o33[0m"
