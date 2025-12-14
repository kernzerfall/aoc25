-- Solution idea: Group theory + Total Variation

phi :: Int -> Int -> Int
phi v p
  | v > 0     = p `div` 100
  | otherwise = negate (negate p `div` 100)

solve :: String -> Int
solve input = 
    let vectors = map parse (lines input)
        path    = scanl (+) 50 vectors
        moves   = zip path (tail path)
    in sum [ abs (phi (e - s) e - phi (e - s) s) | (s, e) <- moves ]

parse (d:n) = if d == 'R' then read n else negate (read n)

main :: IO()
main = do
  interact $ show . solve
