-- Solution idea: combinatorial search

import Data.Char (isDigit)
import Data.Bits (xor, setBit)
import Data.List (find)
import Data.Maybe (mapMaybe)

type Machine = (Integer, [Integer])

extractBetween :: Char -> Char -> String -> [String]
extractBetween open close [] = []
extractBetween open close s =
    case dropWhile (/= open) s of
        [] -> []
        (_:rest) ->
            let (content, remaining) = break (== close) rest
            in content : extractBetween open close remaining

parseMachine :: String -> Machine
parseMachine line =
    let
        targets = extractBetween '[' ']' line
        rawButtons = filter (notElem '{') (extractBetween '(' ')' line)
    in case targets of
        [] -> undefined
        (tStr:_) ->
            let
                targetMask = foldl (\acc (i, c) -> if c == '#' then setBit acc i else acc)
                                   0 (zip [0..] tStr)
                parseBtn s = foldl setBit 0 (map read (words (replacePunctuation s)))
                buttonMasks = map parseBtn rawButtons
            in (targetMask, buttonMasks)

replacePunctuation :: String -> String
replacePunctuation = map (\c -> if isDigit c then c else ' ')

-- We want the MINIMUM presses.
-- Since order doesn't matter and parity is mod 2, we just need a SUBSET of buttons.
-- -> iterate k = 0, 1, 2... and check all subsets of that size.
solveMachine :: Machine -> Int
solveMachine (target, buttons) =
    let
        numButtons = length buttons
        searchSpace = concatMap (`combinations` buttons) [0..numButtons]
        isValid subset = foldl xor 0 subset == target
    in maybe 0 length (find isValid searchSpace)

solve = sum . map (solveMachine . parseMachine) . lines

-- combogen
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x:xs) = map (x:) (combinations (k-1) xs) ++ combinations k xs

main :: IO ()
main = do
    interact $ show . solve
