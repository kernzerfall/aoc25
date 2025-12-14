-- Solution idea: Model the set of all invalid numbers as a union of
-- finite arithmetic progressions.

data Lattice = Lat Integer Integer Integer

lattices :: [Lattice]
lattices = [ Lat step (lower * step) (upper * step) 
           | d <- [1..9]
           , let step  = 10^d + 1
           , let lower = 10^(d-1)
           , let upper = 10^d - 1
           ]

integrate :: (Integer, Integer) -> Lattice -> Integer
integrate (a, b) (Lat step minVal maxVal) =
    let 
        start = max a minVal
        end   = min b maxVal
        
        k_first = (start + step - 1) `div` step
        k_last  = end `div` step
        
        count = k_last - k_first + 1
    in 
        if count <= 0 then 0
        else 
            step * (k_first + k_last) * count `div` 2

parseRanges :: String -> [(Integer, Integer)]
parseRanges input = pairUp tokens
  where
    cleaned = map (\c -> if c == '-' || c == ',' then ' ' else c) input
    tokens  = map read (words cleaned)
    pairUp (x:y:rest) = (x, y) : pairUp rest
    pairUp _          = []

solve :: String -> Integer
solve input = sum [ integrate r lat | r <- parseRanges input, lat <- lattices ]

main :: IO ()
main = do
    interact $ show . solve
