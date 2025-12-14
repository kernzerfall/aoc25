-- Solution idea: The password is the count of the identity element in the
-- scan of the monoid action on the initial state.

rotate pos (dir, amt) = case dir of
    'R' -> (pos + amt) `mod` 100
    'L' -> (pos - amt) `mod` 100

solve initial = length . filter (== 0) . scanl rotate initial

parse :: String -> [(Char, Integer)]
parse = map (\(d:n) -> (d, read n)) . lines

main :: IO()
main = do
  interact $ show . solve 50 . parse
