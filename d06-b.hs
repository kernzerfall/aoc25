-- Solution idea: Reuse the transpose logic from (a), but this time we do
-- not transpose back the stream. Process the columns directly as
-- the numbers themselves.

import Data.List (transpose, isSuffixOf)
import Data.Char (isDigit, isSpace)

-- Splits a list of columns into blocks based on empty separator columns
splitProblems :: [String] -> [[String]]
splitProblems [] = []
splitProblems cols = 
    -- Drop leading empty columns
    case dropWhile (all isSpace) cols of
        [] -> []
        rest -> 
            -- Take columns until the next empty one
            let (problemCols, remaining) = break (all isSpace) rest
            in problemCols : splitProblems remaining

-- Maps a sub-matrix (list of columns) to a result
solveProblem :: [String] -> Int
solveProblem cols = 
    let 
        stream = concat cols
        
        -- operator
        isOp c = c == '+' || c == '*'
        opChar = head $ filter isOp stream
        operator = if opChar == '+' then (+) else (*)
        
        -- operands
        clean c = if isDigit c then c else ' '
        numbers = map read $ words $ map clean stream
        
    in foldl1 operator numbers

solve :: String -> Int
solve input = 
    let 
        rows = lines input
        -- pad to max width
        width = maximum (map length rows)
        paddedRows = map (\r -> r ++ replicate (width - length r) ' ') rows
        
        -- transpose to work with vertical columns
        columns = transpose paddedRows
        
        -- partition into sub-matrices
        problemBlocks = splitProblems columns
        
    in sum $ map solveProblem problemBlocks

main :: IO ()
main = do
    interact $ show . solve
