data Direction = L | R
forward    :: IO ()
obstructed :: IO Bool
turn       :: Direction -> IO ()


robot = do
    sensed <- obstructed
    if sensed 
      then turn L
      else forward
    robot