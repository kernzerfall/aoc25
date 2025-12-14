-- Solution idea: k-Core decomposition

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Graph = M.Map Coord Int -- Maps Coordinate to current Degree

-- Moore Neighborhood
neighbors :: Coord -> [Coord]
neighbors (r, c) = [ (r+dr, c+dc) | dr <- [-1..1], dc <- [-1..1], (dr,dc) /= (0,0) ]

-- Initial graph state
buildGraph :: String -> Graph
buildGraph input =
    let rows = lines input
        coords = [ (r, c) | (r, row) <- zip [0..] rows, (c, val) <- zip [0..] row, val == '@' ]
        coordSet = S.fromList coords
        -- Calculate degree for a single point
        deg p = length [ n | n <- neighbors p, n `S.member` coordSet ]
    in M.fromList [ (p, deg p) | p <- coords ]

-- k-Core Decomposition UwU
peel :: Int -> Graph -> [Coord] -> Graph
peel k graph [] = graph
peel k graph queue =
    let
        -- Remove current failing nodes
        graphWithoutNodes = foldr M.delete graph queue

        -- Calc degree reduction
        damageMap = M.fromListWith (+)
                    [ (n, -1)
                    | r <- queue
                    , n <- neighbors r
                    , n `M.member` graphWithoutNodes
                    ]

        -- reduce & find new fails
        updateAndCheck key damage oldGraph =
            let oldDeg = oldGraph M.! key
                newDeg = oldDeg + damage
                updatedGraph = M.insert key newDeg oldGraph
                -- It's a new failure if it WAS >= k and NOW is < k
                isNewFailure = oldDeg >= k && newDeg < k
            in (updatedGraph, [key | isNewFailure])

        -- update the graph and build next queue
        (nextGraph, nextQueue) = M.foldrWithKey
                                 (\node dmg (g, q) ->
                                    let (g', new) = updateAndCheck node dmg g in (g', new ++ q))
                                 (graphWithoutNodes, [])
                                 damageMap

    in peel k nextGraph nextQueue

solve :: String -> Int
solve input =
    let initialGraph = buildGraph input
        totalNodes   = M.size initialGraph

        -- initial failing nodes (Degree < 4)
        initialQueue = M.keys $ M.filter (< 4) initialGraph

        -- 4-Core
        finalCore    = peel 4 initialGraph initialQueue

    in totalNodes - M.size finalCore

main :: IO ()
main = do
    interact $ show . solve
