module Maze1 where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Data.Matrix
import Debug.Trace
import Text.Read


------------------------------------------------------------------------
-- Constants.
------------------------------------------------------------------------
-- size :: Int
-- size = 25

route :: Int
route = 0

wall :: Int
wall = 1

-- Game display mode.
display :: Display
display = FullScreen

-- Background color.
bgColor :: Color
bgColor = blue

-- Simulation steps per second.
fps :: Int
fps = 1


------------------------------------------------------------------------
--Types
------------------------------------------------------------------------
type Point1 = (Int, Int)

p_in :: Point1
p_in = (1, 1)

-- p_out :: Point1
-- p_out = (size, size)

type Maze = Matrix Int

data AppState = AppState
  {
    maze :: Maze, --Matrix for maze
    user_state :: (Int, Int), -- User state.
    maze_state :: Bool, -- Maze state: False - in work, True - finished
    end :: Bool,  -- End flag: False - user in maze, True - in end point
    rand :: StdGen,
    size :: Int,
    p_out :: Point1,
    played_time :: Int,
    printed_maze :: Bool
  }

------------------------------------------------------------------------
--Events for moving
------------------------------------------------------------------------
handleEvent :: Event -> AppState -> AppState

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) 
  state@(AppState m user True False _ _ p_out_t _ True) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> state
    Just k -> if k == 1 then state
    else if (new_u == p_out_t)
      then (state {end = True})
    else (state {user_state = new_u})
  
    where
      new_u = (fst user - 1, snd user)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) 
  state@(AppState m user True False _ _ p_out_t _ True) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> state
    Just k -> if k == 1 then state
    else if (new_u == p_out_t)
      then (state {end = True})
    else (state {user_state = new_u})
  
    where
      new_u = (fst user + 1, snd user)

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) 
  state@(AppState m user True False _ _ p_out_t _ True) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> state
    Just k -> if k == 1 then state
    else if (new_u == p_out_t)
      then (state {end = True})
    else (state {user_state = new_u})
    
    where
      new_u = (fst user, snd user - 1)

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) 
  state@(AppState m user True False _ _ p_out_t _ True) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> state
    Just k -> if k == 1 then state
    else if (new_u == p_out_t)
      then (state {end = True})
    else (state {user_state = new_u})
  
    where
      new_u = (fst user, snd user + 1)

-- Ignore all other events.
handleEvent _ (state) = state

------------------------------------------------------------------------

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ state@(AppState _ _ False _ _ size_t _ _ False) = let
  m1 = Debug.Trace.trace ("first") (createMaze (1, 1) (size_t, size_t) state) 
  in (state {maze = m1, maze_state = True, played_time = 0, printed_maze = True})

updateApp _ state@(AppState _ _ True True _ _ _ _ _) = state
  
updateApp _ state@(AppState _ _ True False _ _ _ played_time_t True) = 
  Debug.Trace.trace ("stable1") ( (state {played_time = played_time_t + 1})) 

updateApp _ state = state

-- Draw a picture: maze and user position.
drawApp :: AppState -> Picture
drawApp (AppState m user True False _ size_t _ played_time_t True) = 
  Pictures (squares ++ [user_model] ++ [exit] ++ [timer])
  where 
    squares = [checkWall m i j | i <- [((fst user) - 10)..((fst user) + 10)]
      , j <- [((snd user) - 10)..((snd user) + 10)]]
    checkWall :: Maze -> Int -> Int -> Picture
    checkWall m_t i j = 
      case (safeGet i j m_t) of 
        Just 1 -> Color black $ Translate ((-30) * (u_shift_x)) ((-30) * (u_shift_y)) pic 
        Just 0 -> Color white $ Translate ((-30) * (u_shift_x)) ((-30) * (u_shift_y)) pic
        Just k -> Text ("Somthing wrong" ++ show k)
        Nothing -> Color blue $ Translate ((-30) * (u_shift_x)) ((-30) * (u_shift_y)) pic    --error "Error at drawApp"
      where 
        u_shift_x = fromIntegral (fst user) :: Float
        u_shift_y = fromIntegral (snd user) :: Float
        pic = Polygon [(30*a,30*b), (30*a+30, 30*b), 
          (30*a+30, 30*b+30), (30*a,30*b+30)]
        a = fromIntegral i :: Float
        b = fromIntegral j :: Float
    
    user_model = Color green $ u_pic 
      where
        u_pic = Polygon [(10,10), 
          (10, 20), 
          (20, 20), 
          (20, 10)]
        
        -- a = fromIntegral (fst user) :: Float
        -- b = fromIntegral (snd user) :: Float
    
    exit = Color red $ Translate ((-30) * (u_shift_x)) ((-30) * (u_shift_y)) exit_pic
      where
        u_shift_x = fromIntegral (fst user) :: Float
        u_shift_y = fromIntegral (snd user) :: Float
        exit_pic = Polygon [(30*size1, 30*size1),
          (30*size1 + 30, 30*size1), 
          (30*size1 + 30, 30*size1 + 30), 
          (30*size1, 30*size1 + 30)]
        size1 = fromIntegral size_t :: Float
        
    timer = color black $ Translate (-650) 0 $ time_txt
      where
        time_txt = Text (show played_time_t)

drawApp (AppState _ _ True True _ _ _ played_time_t True) = 
  Pictures [win_text, time_text]
  where 
    win_text = Color red $ Translate (-600) 200 $ win_txt
    win_txt = Text "Congrats!"
    time_text = Color red $ translate (-600) 0 time_txt
    time_txt = Text ("Done in " ++ show played_time_t ++ " seconds")
    
drawApp (AppState _ _ False _ _ _ _ _ False) = 
  loading_text
  where 
    loading_text = Color red $ Text ("Loading...")

drawApp _ = Text ("Something")


-- Main function for generation

createMaze :: Point1 -> Point1 -> AppState -> Maze

createMaze _ _ (AppState m _ True _ _ _ _ _ _) = m -- Finish

createMaze p1 p2 (AppState m _ False _ rnd size_t p_out_t _ _)
  | ((fst p2 - fst p1) < 2) || ((snd p2 - snd p1) < 2) = 
    createMaze p1 p2 (AppState m p_in True False rnd size_t p_out_t 0 False)
    
  | otherwise = let
      mW = setW p1 p2 p_n (fst p1, snd p_n) state
      mH = setH p1 p2 p_n (fst p_n, snd p1)( state {maze = mW} )
      m_in = setElem route p_in mH
      m_out = setElem route p_out_t m_in
      
      m1 = createMaze p1 (fst p_n - 1, snd p_n - 1) 
        (state {maze = m_out, rand = newrnd1})
      m2 = createMaze (fst p1, snd p_n + 1) (fst p_n - 1, snd p2) 
        (state {maze = m1, rand = newrnd1} )
      m3 = createMaze (fst p_n + 1, snd p1) (fst p2, snd p_n - 1) 
        (state {maze = m2, rand = newrnd1} ) 
      m4 = createMaze (fst p_n + 1, snd p_n + 1) p2 
        (state {maze = m3, rand = newrnd1 } )
      m_routes = make_route p1 p2 p_n p_n choose_wall 0 (state {maze = m4})
      
      in createMaze p1 p2 (state {maze = m_routes, maze_state = True, rand = newrnd1})
      where 
        (newx, newrnd) = randomR (0, (fst p2) - (fst p1) - 2) rnd
        (newy, newrnd1) = randomR (0, (snd p2) - (snd p1) - 2) newrnd
        p_n = ((fst p1) + newx + 1, (snd p1) + newy + 1)
        state = (AppState m p_in False False rnd size_t p_out_t 0 False)

        (rnd_ind, _) = randomR (0, 3) newrnd1
        choose_wall = create_wall_list rnd_ind
        
create_wall_list :: Int -> [Int]
create_wall_list 0 = [1, 0, 0, 0]        
create_wall_list 1 = [0, 1, 0, 0]
create_wall_list 2 = [0, 0, 1, 0]
create_wall_list 3 = [0, 0, 0, 1]
create_wall_list _ = [0, 0, 0, 0]

        
make_route :: Point1 -> Point1 -> Point1 -> Point1 -> [Int] -> Int -> AppState -> Maze  

make_route _ _ _ _ _ 4 (AppState m _ _ _ _ _ _ _ _) = m

make_route p1 p2 p_n p_cut l_wall i state@(AppState m _ _ _ rnd size_t p_out_t _ _)
  | ((l_wall !! i) == 0) = 
    if (i == 0)
      then let 
      m1 = check_route_up p1 p2 p_n ((fst p_cut - 1), snd p_cut) m
      in make_route p1 p2 p_n p_cut l_wall (i+1) (state {maze = m1})
    else if (i == 1)
      then let 
      m1 = check_route_right p1 p2 p_n (fst p_cut, (snd p_cut) + 1) m
      in make_route p1 p2 p_n p_cut l_wall (i+1) (state {maze = m1})
    else if (i == 2)
      then let
      m1 = check_route_down p1 p2 p_n ((fst p_cut) + 1, snd p_cut) m
      in make_route p1 p2 p_n p_cut l_wall (i+1) (state {maze = m1})
    else if (i == 3)
      then let
      m1 = check_route_left p1 p2 p_n (fst p_cut, (snd p_cut) - 1) m
      in make_route p1 p2 p_n p_cut l_wall (i+1) (state {maze = m1})
    else make_route p1 p2 p_n p_cut l_wall 4 (AppState m p_in False False rnd size_t p_out_t 0 False)

  | otherwise = make_route p1 p2 p_n p_cut l_wall (i+1) 
    (AppState m p_in False False rnd size_t p_out_t 0 False)
  
  where
    check_route_up :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_up p1_t p2_t p_n_t p_cut_t m_t
      | ((l :: Int) + r + u + d > 2) =  
          check_route_up p1_t p2_t p_n_t ((fst p_cut_t - 1), snd p_cut_t) m_t

      | otherwise = setElem route p_cut_t m_t
      where
        u = case (safeGet (fst p_cut_t - 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        d = case (safeGet (fst p_cut_t + 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        l = case (safeGet (fst p_cut_t) (snd p_cut_t - 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        r = case (safeGet (fst p_cut_t) (snd p_cut_t + 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
    
    check_route_right :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_right p1_t p2_t p_n_t p_cut_t m_t
      | ((l :: Int) + r + u + d > 2) =  
          check_route_right p1_t p2_t p_n_t (fst p_cut_t, (snd p_cut_t) + 1) m_t
          
      | otherwise = setElem route p_cut_t m_t
      where
        u = case (safeGet (fst p_cut_t - 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        d = case (safeGet (fst p_cut_t + 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        l = case (safeGet (fst p_cut_t) (snd p_cut_t - 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        r = case (safeGet (fst p_cut_t) (snd p_cut_t + 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1    
    
    check_route_down :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_down p1_t p2_t p_n_t p_cut_t m_t
      | ((l :: Int) + r + u + d > 2) = 
          check_route_down p1_t p2_t p_n_t ((fst p_cut_t + 1), snd p_cut_t) m_t

      | otherwise = setElem route p_cut_t m_t
      where
        u = case (safeGet (fst p_cut_t - 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        d = case (safeGet (fst p_cut_t + 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        l = case (safeGet (fst p_cut_t) (snd p_cut_t - 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        r = case (safeGet (fst p_cut_t) (snd p_cut_t + 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1    
    
    check_route_left :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_left p1_t p2_t p_n_t p_cut_t m_t
      | ((l :: Int) + r + u + d > 2) =  
          check_route_left p1_t p2_t p_n_t (fst p_cut_t, (snd p_cut_t) - 1) m_t

      | otherwise = setElem route p_cut_t m_t
      where
        u = case (safeGet (fst p_cut_t - 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        d = case (safeGet (fst p_cut_t + 1) (snd p_cut_t) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        l = case (safeGet (fst p_cut_t) (snd p_cut_t - 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1
        r = case (safeGet (fst p_cut_t) (snd p_cut_t + 1) m_t) of 
          Just 1 -> 1
          Just 0 -> 0
          Just _ -> 1
          Nothing -> 1  
  
setW :: Point1 -> Point1 -> Point1 -> Point1 -> AppState -> Maze
setW p1 p2 p_n set_point (AppState m _ _ _ rand_t size_t p_out_t _ _) 
  | ((fst set_point) > (fst p2)) = m
  | otherwise = let
    m1 = setElem wall set_point m
    in setW p1 p2 p_n ((fst set_point) + 1, snd set_point) 
      (AppState m1 p_in False False rand_t size_t p_out_t 0 False)
  
setH :: Point1 -> Point1 -> Point1 -> Point1 -> AppState -> Maze
setH p1 p2 p_n set_point (AppState m _ _ _ rand_t size_t p_out_t _ _)
  | ((snd set_point) > (snd p2)) = m
  | otherwise = let
    m1 = setElem wall set_point m
    in setH p1 p2 p_n (fst set_point, (snd set_point) + 1) 
      (AppState m1 p_in False False rand_t size_t p_out_t 0 False)
      
------------------------------------------------------------------------
--Other finctions.
------------------------------------------------------------------------

parseCmd :: [String] -> Maybe Int
parseCmd [n] = 
  case (readMaybe n :: Maybe Int) of
  Nothing -> Nothing
  Just num -> if (num <= 2) then Nothing else Just num
parseCmd _ = Nothing


run :: IO()
run = do
  args <- System.Environment.getArgs
  case parseCmd args of 
    -- Check if the args have correct format
    Nothing -> putStrLn "Wrong args, size must be more than 2"
    Just size_read -> do
      rndGen <- newStdGen  

      play display bgColor fps (AppState (zero size_read size_read) 
        p_in False False rndGen size_read (size_read, size_read) 0 False)
          drawApp handleEvent updateApp