{-# OPTIONS_GHC -Wall #-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id,(*2)]) . reverse