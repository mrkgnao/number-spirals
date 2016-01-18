module Spiral where

import Data.List (intercalate)

data RelativePosition = Pre | Post

appendCharsToArray :: RelativePosition -> [String] -> [String] -> [String]
appendCharsToArray _ [] [] = []
appendCharsToArray pos (c:cs) (x:xs) = cur:rest
  where rest = appendCharsToArray pos cs xs
        cur = concat $ case pos of
                Pre ->  [c,x]
                Post -> [x,c]

spiralArr :: Int -> Int -> (Int -> String) -> [String]
spiralArr 1 p f = [centerPad p (show' 0)]
  where show' n = if (f n == "") then (show n) else (f n)
spiralArr n p f =
  let
    show' n = if (f n == "") then (show n) else (f n)
    fixFunc  = if (odd n) then reverse else id
    start = (n-1)^2
    mid   = start + (n-1)
    end   = n^2 - 1
    side  = fixFunc $ map (centerPad p . show') [start .. mid-1]
    row   = fixFunc $ map show' [mid .. end]
    row'  = concatMap (centerPad p) row
    rest  = spiralArr (n-1) p f
  in
    -- Pad the appropriate side with (n-1)^2 to (n-1)^2 + (n-1)
    -- and add a new row to either the beginning or the end
    case odd n of
      -- UL -> DL -> DR
      True -> row' : (appendCharsToArray Post side rest)
      False -> (appendCharsToArray Pre side rest) ++ [row']

spiralFunc f n = putStrLn . unlines $ spiralArr n (digits n + 2) f
plainSpiral = spiralFunc (const "")
primeSpiral = spiralFunc starPrimeNumbers
  where starPrimeNumbers n = if (isPrime n) then "*" else ""
        isPrime n
          | n < 2 = False
          | otherwise = length (filter (\x -> n `mod` x == 0) [2..n-1]) == 0
modSpiral m = spiralFunc (\x -> if (x `mod` m == 0) then "*" else "")

-- Utterly uninteresting utility functions
digits n = floor (logBase 10 (fromIntegral n)) + 1
padLength len str = (len - (length str))
rightPadding len str = concat $ replicate l " "
  where l = (padLength len str) `quot` 2
leftPadding len str = concat $ replicate l " "
  where l' = padLength len str
        l = l' - (l' `quot` 2)
centerPad len str = leftPadding len str ++ str ++ rightPadding len str
