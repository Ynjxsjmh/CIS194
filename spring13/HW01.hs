{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = [n `mod` 10] ++ toDigitsRev (n `div` 10)

-- Exercise 2 -----------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith ($) (cycle [id,(*2)]) . reverse

-- Exercise 3 -----------------------------------------

sumOneDigits :: [Integer] -> Integer
sumOneDigits [] = 0
sumOneDigits (x:xs) = x + sumOneDigits xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumOneDigits (toDigits x) + sumDigits xs

-- Exercise 4 -----------------------------------------

validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther  (toDigits n))) `mod` 10 == 0

-- Exercise 5 -----------------------------------------

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n == 1 = [(a,b)]
  | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a