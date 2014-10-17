import qualified Data.Set as Set

type Cell = (Int, Int)
type Grid = Set.Set Cell

neighbors_of_a_cell :: Cell -> [Cell]
neighbors_of_a_cell (x, y) = [(x + a, y + b) | a <- [-1..1], b <- [-1..1], a /= 0 || b /= 0]

direct_surroundings_of :: Grid -> Grid
direct_surroundings_of alive_cells =
  Set.difference
  (Set.fromList $ concatMap neighbors_of_a_cell $ Set.toList alive_cells)
  alive_cells

alive_neighbor_count :: Grid -> Cell -> Int
alive_neighbor_count alive_cells (x, y) =
  length $ filter (`Set.member` alive_cells) $ neighbors_of_a_cell (x, y)

alive_cells_that_will_stay_alive :: Grid -> Grid
alive_cells_that_will_stay_alive alive_cells =
  Set.fromList $
    filter ((`elem` [2,3]) . alive_neighbor_count alive_cells) $
      Set.toList alive_cells

dead_cells_that_will_come_alive :: Grid -> Grid
dead_cells_that_will_come_alive alive_cells =
  Set.fromList $
    filter ((== 3) . alive_neighbor_count alive_cells) $
      Set.toList $
        direct_surroundings_of alive_cells

evolve_universe :: Grid -> Grid
evolve_universe alive_cells =
  Set.union
  (alive_cells_that_will_stay_alive alive_cells)
  (dead_cells_that_will_come_alive  alive_cells)

main = do
  putStrLn "Dupa []:"
  print $ evolve_universe $ Set.fromList []

  putStrLn "Dupa [(0,0)]:"
  print $ evolve_universe $ Set.fromList [(0,0)]

  putStrLn "Dupa [(0,0),(0,1)]:"
  print $ evolve_universe $ Set.fromList [(0,0),(0,1)]

  putStrLn "Dupa [(0,0),(0,1),(1,0)]:"
  print $ evolve_universe $ Set.fromList [(0,0),(0,1),(1,0)]

  putStrLn "Dupa [(1,0),(2,1),(0,2),(1,2),(2,2)]:"
  print $ evolve_universe $ Set.fromList [(1,0),(2,1),(0,2),(1,2),(2,2)]

  putStrLn "Dupa [(0,1),(1,2),(1,3),(2,1),(2,2)]:"
  print $ evolve_universe $ Set.fromList [(0,1),(1,2),(1,3),(2,1),(2,2)]

