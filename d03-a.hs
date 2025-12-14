-- Solution idea: supremum of interval topology of the input

import qualified Data.Map.Strict as M
import Data.List (find)

-- Maps a digit to its (first, last appearance)
type Bounds = M.Map Char (Int, Int)

-- Semigroup action updating the min/max state
buildBounds :: String -> Bounds
buildBounds s = fst $ foldl update (M.empty, 0) s
  where
    update (acc, idx) char = 
        let newBounds = case M.lookup char acc of
                Nothing      -> (idx, idx) -- First time seeing char
                Just (mn, _) -> (mn, idx)  -- Update max only
        in (M.insert char newBounds acc, idx + 1)

-- Checks candidates in descending order
-- Returns the first one that satisfies the temporal constraint
solveBank :: String -> Int
solveBank s = 
    let bounds = buildBounds s
        
        isValid (x, y) = case (M.lookup x bounds, M.lookup y bounds) of
            (Just (minX, _), Just (_, maxY)) -> minX < maxY
            _ -> False

        candidates = [ (x, y) | x <- d , y <- d ] where d = "987654321"
        
    in case find isValid candidates of
        Just (d1, d2) -> read [d1, d2]
        Nothing       -> 0

solve :: String -> Int
solve input = sum $ map solveBank (lines input)

main :: IO ()
main = do
    interact $ show . solve
