-- Solution idea: Backward induction on DAG.

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.List (sort, sortOn, find)
import Data.Ord (Down(..))

type Coord = (Int, Int)
type ColumnMap = IM.IntMap [Int] -- Column -> Sorted list of Rows
type Cache = M.Map Coord Integer  -- Splitter Coord -> Number of Timelines

-- Raycaster
-- Given starting position (r, c), find first splitter directly below it.
findHit :: ColumnMap -> Coord -> Maybe Coord
findHit splitters (r, c) = 
    case IM.lookup c splitters of
        Nothing -> Nothing
        Just rows -> 
            -- Find the smallest row r' > r
            case find (> r) rows of
                Nothing -> Nothing -- Beam exits the manifold
                Just hitR -> Just (hitR, c)

-- Backward Induction
solve :: String -> Integer
solve input = 
    let 
        rows = lines input
        cells = [ ((r, c), char) 
                | (r, row) <- zip [0..] rows
                , (c, char) <- zip [0..] row
                , char == 'S' || char == '^' 
                ]
        
        startPos = fst $ head $ filter ((== 'S') . snd) cells
        splitterCoords = map fst $ filter ((== '^') . snd) cells
        
        -- Build Sparse Column Index
        colMap = IM.fromListWith (++) [ (c, [r]) | (r, c) <- splitterCoords ]
        sortedColMap = IM.map sort colMap
        
        -- Sort splitters Bottom-Up
        -- -> always process children before parents
        orderedSplitters = sortOn (Down . fst) splitterCoords
        
        -- timeline count for each splitter
        calcTimelines :: Cache -> Coord -> Cache
        calcTimelines cache (r, c) = 
            let 
                leftHit  = findHit sortedColMap (r, c - 1)
                rightHit = findHit sortedColMap (r, c + 1)
                
                -- Value lookup helper:
                -- If hit is Nothing (exit), count is 1.
                -- If hit is Just node, look it up in cache.
                val hit = case hit of
                    Nothing -> 1
                    Just h  -> cache M.! h
                
                total = val leftHit + val rightHit
            in M.insert (r, c) total cache

        -- Backward Induction, kinda
        finalCache = foldl calcTimelines M.empty orderedSplitters
        
        -- Finally, trace the path from Source S
        startHit = findHit sortedColMap startPos
        
    in case startHit of
        Nothing -> 1 -- S points straight to exit
        Just h  -> finalCache M.! h

main :: IO ()
main = do
    interact $ show . solve
