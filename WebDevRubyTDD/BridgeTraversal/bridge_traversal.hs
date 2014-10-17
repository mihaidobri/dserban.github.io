import Data.Maybe

data Scandura
  = SB -- scândură bună
  | SD -- scândură deteriorată
  | SL -- scândură lipsă
  deriving ( Show )

data Pas
  = SB1SB
  | SB2SB
  | SB3SB
  | SB1SD
  | SD1SB
  deriving ( Show )

generare_pas :: Scandura -> Scandura -> Int -> Maybe Pas
generare_pas SL _  _ = Nothing
generare_pas _  SL _ = Nothing
generare_pas SB SD 1 = Just SB1SD
generare_pas SB SD _ = Nothing
generare_pas SD SB 1 = Just SD1SB
generare_pas SD SB _ = Nothing
generare_pas SB SB 1 = Just SB1SB
generare_pas SB SB 2 = Just SB2SB
generare_pas SB SB 3 = Just SB3SB

solution_space :: [Scandura] -> [[Maybe Pas]]
solution_space [s1,s2] =
  filter (all isJust) [ [generare_pas s1 s2 1] ]
solution_space [s1,s2,s3] =
  filter (all isJust) [ [generare_pas s1 s2 1, generare_pas s2 s3 1]
                      , [generare_pas s1 s3 2]
                      ]
solution_space [s1,s2,s3,s4] =
  filter (all isJust) [ [generare_pas s1 s2 1, generare_pas s2 s3 1, generare_pas s3 s4 1]
                      , [generare_pas s1 s2 1, generare_pas s2 s4 2]
                      , [generare_pas s1 s3 2, generare_pas s3 s4 1]
                      , [generare_pas s1 s4 3]
                      ]
solution_space (s1:s2list@(s2:s3list@(s3:s4list@(s4:s5list)))) =
  filter (all isJust) $
    map (s1s2:) (solution_space s2list) ++
    map (s1s3:) (solution_space s3list) ++
    map (s1s4:) (solution_space s4list)
  where
    s1s2 = generare_pas s1 s2 1
    s1s3 = generare_pas s1 s3 2
    s1s4 = generare_pas s1 s4 3

valid_solutions :: [Scandura] -> [[Pas]]
valid_solutions = map catMaybes . solution_space

main = do
  putStrLn ""

  putStrLn "Traversari posibile pentru [SB,SB]:"
  mapM_ print $ valid_solutions [SB,SB]
  putStrLn ""

  putStrLn "Traversari posibile pentru [SB,SB,SB]:"
  mapM_ print $ valid_solutions [SB,SB,SB]

  putStrLn ""

  putStrLn "Traversari posibile pentru [SB,SD,SB,SB]:"
  mapM_ print $ valid_solutions [SB,SD,SB,SB]

  putStrLn ""

  putStrLn "Traversari posibile pentru [SB,SD,SB,SL,SB]:"
  mapM_ print $ valid_solutions [SB,SD,SB,SL,SB]

  putStrLn ""

  putStrLn "Traversari posibile pentru [SB,SL,SB,SB,SL,SB,SD,SB]:"
  mapM_ print $ valid_solutions [SB,SL,SB,SB,SL,SB,SD,SB]

  putStrLn ""

