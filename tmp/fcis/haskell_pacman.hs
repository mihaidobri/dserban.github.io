import qualified Data.Map as Map

data PacmanDirection = Forward | Backward deriving (Show)

data Location = A1 | A2 | A3 | A4 | A5 | B5 | C5 | D5 | E5 | F5 | G5 | H5 | B1 | C1 | D1 | E1 | F1 | G1 | H1 | I1 | I2 | I3 | I4 | I5 | E2 | E3 | E4 | ZZ deriving (Eq,Ord,Show)

data GameState = GameState { pacman_direction :: PacmanDirection
                           , pacman_location  :: Location
                           , ghost_location   :: Location
                           } deriving (Show)

pacman_forward_path = Map.fromList [ (A5,A4)
                                   , (A4,A3)
                                   , (A3,A2)
                                   , (A2,A1)
                                   , (A1,B1)
                                   , (B1,C1)
                                   , (C1,D1)
                                   , (D1,E1)
                                   , (E1,E2)
                                   , (E2,E3)
                                   , (E3,E4)
                                   , (E4,E5)
                                   , (E5,F5)
                                   , (F5,G5)
                                   , (G5,H5)
                                   , (H5,I5)
                                   , (I5,I4)
                                   , (I4,I3)
                                   , (I3,I2)
                                   , (I2,I1)
                                   ]

pacman_backward_path = Map.fromList [ (I1,I2)
                                    , (I2,I3)
                                    , (I3,I4)
                                    , (I4,I5)
                                    , (I5,H5)
                                    , (H5,G5)
                                    , (G5,F5)
                                    , (F5,E5)
                                    , (E5,E4)
                                    , (E4,E3)
                                    , (E3,E2)
                                    , (E2,E1)
                                    , (E1,D1)
                                    , (D1,C1)
                                    , (C1,B1)
                                    , (B1,A1)
                                    , (A1,A2)
                                    , (A2,A3)
                                    , (A3,A4)
                                    , (A4,A5)
                                    ]

ghost_path = Map.fromList [ (I1,I2)
                          , (I2,I3)
                          , (I3,I4)
                          , (I4,I5)
                          , (I5,H5)
                          , (H5,G5)
                          , (G5,F5)
                          , (F5,E5)
                          , (E5,D5)
                          , (D5,C5)
                          , (C5,B5)
                          , (B5,A5)
                          , (A5,A4)
                          , (A4,A3)
                          , (A3,A2)
                          , (A2,A1)
                          , (A1,B1)
                          , (B1,C1)
                          , (C1,D1)
                          , (D1,E1)
                          , (E1,F1)
                          , (F1,G1)
                          , (G1,H1)
                          , (H1,I1)
                          ]

pacman_on_clock_tick :: (PacmanDirection, Location) -> (PacmanDirection, Location)
pacman_on_clock_tick (Forward, ZZ) = (Forward,  ZZ)
pacman_on_clock_tick (Forward, I2) = (Backward, I1)
pacman_on_clock_tick (Forward, location) =
                     (Forward, (maybe ZZ id . pacman_next_forward_location) location)
  where
    pacman_next_forward_location location = Map.lookup location pacman_forward_path
pacman_on_clock_tick (Backward, ZZ) = (Backward, ZZ)
pacman_on_clock_tick (Backward, A4) = (Forward,  A5)
pacman_on_clock_tick (Backward, location) =
                     (Backward, (maybe ZZ id . pacman_next_backward_location) location)
  where
    pacman_next_backward_location location = Map.lookup location pacman_backward_path

ghost_on_clock_tick = maybe ZZ id . ghost_next_location
  where
    ghost_next_location location = Map.lookup location ghost_path

gameplay_on_clock_tick gs = GameState { pacman_direction = fst next_pacman
                                      , pacman_location  = snd next_pacman
                                      , ghost_location   = (ghost_on_clock_tick . ghost_location) gs
                                      }
  where
    next_pacman = pacman_on_clock_tick (pacman_direction gs, pacman_location gs)

initial_game_state = GameState { pacman_direction = Forward
                               , pacman_location  = A5
                               , ghost_location   = I1
                               }

-- last . (take 4) $ iterate gameplay_on_clock_tick initial_game_state


