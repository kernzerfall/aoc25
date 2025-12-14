-- Solution idea: Lebesgue Measure of the union of the intervals
-- over the integer lattice.

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

-- Maps a closed interval to its cardinality in Z
measure :: Interval -> Int
measure (s, e) = e - s + 1

solve :: String -> Int
solve input = 
    let 
        -- ignore the query block below blank line
        rangeBlock = takeWhile (/= "") (lines input)
        
        parseRange s = let (a, b) = break (== '-') s in (read a, read (tail b))
        rawRanges = map parseRange rangeBlock
        
        basis = normalize rawRanges
        
    in sum (map measure basis)

main :: IO ()
main = do
    interact $ show . solve
