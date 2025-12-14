-- Solution idea: Reachability analysis on a DAG

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as Set
import Data.List (sort, find)

-- Maps a Column Index to a sorted list of Row Indices where splitters exist
type ColumnMap = IM.IntMap [Int]
type Coord = (Int, Int)

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

-- Computes all reachable splitters
explore :: ColumnMap -> Set.Set Coord -> [Coord] -> Int
explore _ visited [] = Set.size visited
explore splitters visited (current:queue) = 
    let 
        (r, c) = current
        
        leftTarget  = findHit splitters (r, c - 1)
        rightTarget = findHit splitters (r, c + 1)
        
        -- Filter neighbors: Must exist and not be visited yet
        nextNodes = [ n | Just n <- [leftTarget, rightTarget],
                                     not (Set.member n visited) ]
        
        -- Update state
        newVisited = foldr Set.insert visited nextNodes
        newQueue   = nextNodes ++ queue
        
    in explore splitters newVisited newQueue

solve :: String -> Int
solve input = 
    let 
        rows = lines input
        -- coordinates of 'S' and all '^'
        cells = [ ((r, c), char) 
                | (r, row) <- zip [0..] rows
                , (c, char) <- zip [0..] row
                , char == 'S' || char == '^' 
                ]
        
        -- Start and Splitters
        startPos = fst $ head $ filter ((== 'S') . snd) cells
        splitterCoords = map fst $ filter ((== '^') . snd) cells
        
        -- Build Column Index
        -- Insert row 'r' into list at key 'c'
        colMap = IM.fromListWith (++) [ (c, [r]) | (r, c) <- splitterCoords ]
        -- Ensure lists are sorted for the 'find' operation
        sortedColMap = IM.map sort colMap
        
        -- Determine the root of the interaction tree
        initialHit = findHit sortedColMap startPos
        
    in case initialHit of
        Nothing -> 0 -- S points into empty space
        Just root -> explore sortedColMap (Set.singleton root) [root]

main :: IO ()
main = do
    interact $ show . solve
