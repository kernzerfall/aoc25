-- Solution idea: Combinatorial Inclusion-Exclusion principle.

-- Generates Repunit steps: (10^(kd) - 1) / (10^d - 1)
-- Returns (Step Size, Lower Bound, Upper Bound)
generateProgression :: Int -> Int -> (Integer, Integer, Integer)
generateProgression k d = 
    let base  = 10^d
        lower = 10^(d-1)
        upper = base - 1
        -- Geometric Series Sum formula for repeating 'k' times in base 10^d
        step  = (10^(k*d) - 1) `div` (base - 1)
    in (step, lower * step, upper * step)

-- Calculates contribution of a specific repetition mode k over a range [a,b]
integrate :: (Integer, Integer) -> Int -> Integer -> Integer
integrate (a, b) k sign = 
    -- Sum over all possible "seed lengths" d
    -- s.t. 10^(k*d - 1) does not exceed b
    sum [ calc d | d <- [1..30], 10^fromIntegral (k*d-1) <= b ]
  where
    calc d = 
        let (step, minVal, maxVal) = generateProgression k d
            start = max a minVal
            end   = min b maxVal
            
            k_first = (start + step - 1) `div` step
            k_last  = end `div` step
            count   = k_last - k_first + 1
        in 
            if count <= 0 then 0
            else sign * step * (k_first + k_last) * count `div` 2

-- Generates all subsets of primes whose product is relevant
-- For very large numbers, we only check primes that keep total digits reasonable.
solveRange :: (Integer, Integer) -> Integer
solveRange rng = sum [ integrate rng k sign | (k, sign) <- inclusionTerms ]

-- Pre-computed inclusion/exclusion terms based on primes
inclusionTerms :: [(Int, Integer)]
inclusionTerms = 
    let primes = [2, 3, 5, 7, 11, 13, 17, 19, 23]
        -- Recursive powerset generator that tracks the Product and the Sign
        -- go [primes] currentProduct currentSign
        go [] _ _ = []
        go (p:ps) prod sgn = 
             let newProd = prod * p
                 newSgn  = negate sgn
                 -- If product is too large, prune branch
                 current = if newProd < 60 then (newProd, newSgn) : go ps newProd newSgn else []
             in current ++ go ps prod sgn
    in go primes 1 (-1) -- Start with sign -1 so the first flip makes it +1

-- Parse n stuff
parseRanges :: String -> [(Integer, Integer)]
parseRanges input = pairUp tokens
  where
    cleaned = map (\c -> if c == '-' || c == ',' then ' ' else c) input
    tokens  = map read (words cleaned)
    pairUp (x:y:rest) = (x, y) : pairUp rest
    pairUp _          = []

solve :: String -> Integer
solve input = sum [ solveRange r | r <- parseRanges input ]

main :: IO ()
main = do
    interact $ show . solve
