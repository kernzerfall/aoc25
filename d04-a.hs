-- Solution idea: Degree distribution of induced subgraphs

import qualified Data.Set as S

-- Moore Neighborhood Calc
directions :: [(Int, Int)]
directions = [ (r, c) | r <- [-1..1], c <- [-1..1], (r,c) /= (0,0) ]

-- Maps a coordinate 'p' to its list of 8 adjacent coordinates
neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [ (r+dr, c+dc) | (dr, dc) <- directions ]

-- Count how many neighbors of 'p' are present in the set 'grid'
degree :: S.Set (Int, Int) -> (Int, Int) -> Int
degree grid p = length $ filter (`S.member` grid) (neighbors p)

solve :: String -> Int
solve input = 
    let 
        -- Parse into a sparse set of coordinates
        rows = lines input
        coords = [ (r, c) 
                 | (r, row) <- zip [0..] rows
                 , (c, val) <- zip [0..] row
                 , val == '@' ]
        grid = S.fromList coords

        -- Filter nodeset based on the degree property in the induced subgraph
        accessible = S.filter (\p -> degree grid p < 4) grid
    in 
        S.size accessible

main :: IO ()
main = do
    interact $ show . solve
