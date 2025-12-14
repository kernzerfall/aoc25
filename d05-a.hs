-- Solution idea: Convert interval union to canonical form and do
-- straightforward membership test

import Data.List (sort, foldl')

type Interval = (Int, Int)

-- Transforms a set of overlapping ranges into 
-- the minimal set of disjoint intervals with same coverage.
normalize :: [Interval] -> [Interval]
normalize ranges =
    let sorted = sort ranges
    in foldl' merge [] sorted
  where
    merge [] next = [next]
    merge ((s, e):rest) (ns, ne)
        | ns <= e + 1 = (s, max e ne) : rest
        | otherwise   = (ns, ne) : (s, e) : rest

-- Membership Test
-- Since normalized intervals are sorted (descending in the fold accumulator),
-- we can check efficiently. 
inIntervals :: Int -> [Interval] -> Bool
inIntervals x = any (\(s, e) -> x >= s && x <= e)

solve :: String -> Int
solve input =
    let
        -- Split input into (Ranges, Queries)
        (rangeBlock, queryBlock) = break (== "") (lines input)

        parseRange s = let (a, b) = break (== '-') s in (read a, read (tail b))
        rawRanges = map parseRange rangeBlock

        queries = map read (drop 1 queryBlock) :: [Int]

        canonicalSet = normalize rawRanges
    in length $ filter (`inIntervals` canonicalSet) queries

main :: IO ()
main = do
    interact $ show . solve
