{- |
Module      : ColourStrings
Description : Helper functions for colouring output
License     : BSD-3-Clause
Maintainer  : surplussinewaves@gmail.com
-}
module ColourStrings where

-- | Wrap a string in the escape sequences for red
toRed :: String -> String
toRed s = "\o33[31m" ++ s ++ "\o33[0m"

-- | Wrap a string in the escape sequences for blue
toBlue :: String -> String
toBlue s = "\o33[34m" ++ s ++ "\o33[0m"

-- | Wrap a string in the escape sequences for magenta
toMagenta :: String -> String
toMagenta s = "\o33[35m" ++ s ++ "\o33[0m"

-- | Wrap a string in the escape sequences for cyan
toCyan :: String -> String
toCyan s = "\o33[36m" ++ s ++ "\o33[0m"

-- | Wrap a string in the escape sequences for green
toGreen :: String -> String
toGreen s = "\o33[32m" ++ s ++ "\o33[0m"
