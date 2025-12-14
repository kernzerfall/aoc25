-- Solution idea: See "lexicographically largest subsequence of fixed length
-- problem"

-- We select 'k' items from the list 'xs' to maximize the resulting sequence
maximize :: Int -> String -> String
maximize 0 _ = []
maximize k xs =
    let
        -- We can only search in the prefix that leaves enough room
        -- for the remaining (k-1) items.
        searchWindowSize = length xs - k + 1
        searchWindow     = take searchWindowSize xs

        -- Find the supremum in the allowed window
        bestDigit = maximum searchWindow

        -- Jump to the state immediately following the selection
        -- 'break' finds the first occurrence, ensuring we save as much
        -- string as possible
        (_, _:futureState) = break (== bestDigit) xs
    in
        bestDigit : maximize (k - 1) futureState

solve :: String -> Integer
solve input = sum [ read (maximize 12 line) | line <- lines input ]

main :: IO ()
main = do
    interact $ show . solve
