module Maze1 where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Data.Matrix
import Debug.Trace


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
    p_out = (size, size)
  }

------------------------------------------------------------------------
--Events for moving
------------------------------------------------------------------------
handleEvent :: Event -> AppState -> AppState

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) 
  state@(AppState m user True False rand _ _) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> state
    Just k -> if k == 1 then state
    else if (new_u == p_out)
      then (state {end = True})   AppState m user True True rand
    else AppState m new_u True False rand
  
    where
      new_u = (fst user - 1, snd user)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) 
  (AppState m user True False rand) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False rand
    Just k -> if k == 1 then AppState m user True False rand
    else if (new_u == p_out)
      then AppState m user True True rand
    else AppState m new_u True False rand
  
    where
      new_u = (fst user + 1, snd user)

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) 
  (AppState m user True False rand) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False rand
    Just k -> if k == 1 then AppState m user True False rand
    else if (new_u == p_out)
      then AppState m user True True rand
    else AppState m new_u True False rand
  
    where
      new_u = (fst user, snd user - 1)

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) 
  (AppState m user True False rand) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False rand
    Just k -> if k == 1 then AppState m user True False rand
    else if (new_u == p_out)
      then AppState m user True True rand
    else AppState m new_u True False rand
  
    where
      new_u = (fst user, snd user + 1)

-- Ignore all other events.
handleEvent _ (state) = state

------------------------------------------------------------------------

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ state@(AppState m user False _ _) = let
  m = Debug.Trace.trace ("first") (createMaze (1, 1) (size, size) state) 
  in (state {maze = m, maze_state = True})

updateApp _ state@(AppState m user True True rnd) = let
  state1 = AppState (zero size size) p_in False False rnd
  m1 = Debug.Trace.trace ("expected new") (createMaze (1, 1) (size, size) state1)
  in (state {maze = m1, maze_state = True})
  
updateApp _ state@(AppState m user True False _) = Debug.Trace.trace ("stable") (state)


-- Draw a picture: maze and user position.
drawApp :: AppState -> Picture
drawApp (AppState m user True False _) = Pictures (squares ++ [user_model] ++ [exit])
  where 
    squares = [checkWall m i j | i <- [1..size], j <- [1..size]]
    checkWall :: Maze -> Int -> Int -> Picture
    checkWall m i j = 
      case (safeGet i j m) of 
        Just 1 -> Color black $ Translate (-250) (-250) pic 
        Just 0 -> Color white $ Translate (-250) (-250) pic
        Nothing -> error "Error at drawApp"
      where 
        size1 = fromIntegral size :: Float
        len = 300 / size1
        pic = Polygon [(a + len*a,b + len*b), (a+len*a+len, b+len*b), 
          (a+len*a+len, b+len*b+len), (a+len*a, b+len*b+len)]
        a = fromIntegral i :: Float
        b = fromIntegral j :: Float
    
    user_model = Color green $ Translate (-250) (-250) u_pic 
      where
        size1 = fromIntegral size :: Float
        len = 300 / size1
        u_pic = Polygon [(a + (len*a),b + (len*b)), (a+ (len / 2) + (len*a), b + (len*b)), 
          (a + (len*a)+ (len / 2), b+ (len*b)+ (len / 2)), (a + (len*a), b + (len*b) + (len / 2))]
        a = fromIntegral (fst user) :: Float
        b = fromIntegral (snd user) :: Float
    exit = Color red $ Translate (-250) (-250) exit_pic
      where
        size2 = fromIntegral size :: Float
        len = 300 / size2
        exit_pic = Polygon [((len + 1)*size1, (len + 1)*size1), ((len + 1)*size1 + len, (len + 1)*size1), 
          ((len + 1)*size1 + len, (len + 1)*size1 + len), ((len + 1)*size1, (len + 1)*size1 + len)]
        size1 = fromIntegral size :: Float

drawApp (AppState m user True True _) = win_text
  where 
    win_text = Color red $ Translate 0 0 $ txt
    txt = Text "Congrats!"

    
drawApp (AppState m user False _ _) = Debug.Trace.trace("false maze_state" ++ show m) (Pictures squares)
  where 
    squares = [checkWall m i j | i <- [1..size], j <- [1..size]]
    checkWall :: Maze -> Int -> Int -> Picture
    checkWall m i j = 
      case (safeGet i j m) of 
        Just 1 -> Color black $ pic 
        Just 0 -> Color red $ pic
        Nothing -> error "Error at drawApp"
      where 
        size1 = fromIntegral size :: Float
        len = 300 / size1
        pic = Polygon [(a + len*a,b + len*b), (a+len*a+len, b+len*b), 
          (a+len*a+len, b+len*b+len), (a+len*a, b+len*b+len)]
        a = fromIntegral i :: Float
        b = fromIntegral j :: Float
    

-- Main function for generation
-- лабиринт без стен в матрице
-- внешние стены не учитываются - отрисовать в run do where?

createMaze :: Point1 -> Point1 -> AppState -> Maze

createMaze _ _ (AppState m _ True _ _) = m -- Finish

createMaze p1 p2 (AppState m _ False _ rnd)
  | ((fst p2 - fst p1) < 2) || ((snd p2 - snd p1) < 2) = 
    createMaze p1 p2 (AppState m p_in True False rnd)
    
  | otherwise = let
      mW = setW p1 p2 p_n (fst p1, snd p_n) state
      mH = setH p1 p2 p_n (fst p_n, snd p1)( state {maze = mW} )
      m_in = setElem route p_in mH
      m_out = setElem route p_out m_in
      
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
      -- randomR (0, (fst p2) - (fst p1) - 2)
      -- randomR ((fst p1) + 2, (fst p2) - 2)
        (newx, newrnd) = randomR (0, (fst p2) - (fst p1) - 2) rnd
        (newy, newrnd1) = randomR (0, (snd p2) - (snd p1) - 2) newrnd
        p_n = ((fst p1) + newx + 1, (snd p1) + newy + 1)
        state = (AppState m p_in False False rnd)
         -- прорубить стены, where
        (rnd_ind, newrnd2) = randomR (0, 3) newrnd1
        choose_wall = create_wall_list rnd_ind
        
create_wall_list :: Int -> [Int]
create_wall_list 0 = [1, 0, 0, 0]        
create_wall_list 1 = [0, 1, 0, 0]
create_wall_list 2 = [0, 0, 1, 0]
create_wall_list 3 = [0, 0, 0, 1]

        
make_route :: Point1 -> Point1 -> Point1 -> Point1 -> [Int] -> Int -> AppState -> Maze  

make_route p1 p2 p_n p_cut l_wall 4 (AppState m _ _ _ rnd) = m

make_route p1 p2 p_n p_cut l_wall i state@(AppState m _ _ _ rnd)
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
    else make_route p1 p2 p_n p_cut l_wall 4 (AppState m p_in False False rnd)

  | otherwise = make_route p1 p2 p_n p_cut l_wall (i+1) (AppState m p_in False False rnd)
  
  where
    check_route_up :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_up p1 p2 p_n p_cut m
      | (l + r + u + d > 2) =  
          check_route_up p1 p2 p_n ((fst p_cut - 1), snd p_cut) m

      | otherwise = setElem route p_cut m
      where
        u = case (safeGet (fst p_cut - 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        d = case (safeGet (fst p_cut + 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        l = case (safeGet (fst p_cut) (snd p_cut - 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        r = case (safeGet (fst p_cut) (snd p_cut + 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
    
    check_route_right :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_right p1 p2 p_n p_cut m
      | (l + r + u + d > 2) =  
          check_route_right p1 p2 p_n (fst p_cut, (snd p_cut) + 1) m
          
      | otherwise = setElem route p_cut m
      where
        u = case (safeGet (fst p_cut - 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        d = case (safeGet (fst p_cut + 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        l = case (safeGet (fst p_cut) (snd p_cut - 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        r = case (safeGet (fst p_cut) (snd p_cut + 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1    
    
    check_route_down :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_down p1 p2 p_n p_cut m
      | (l + r + u + d > 2) = 
          check_route_down p1 p2 p_n ((fst p_cut + 1), snd p_cut) m

      | otherwise = setElem route p_cut m
      where
        u = case (safeGet (fst p_cut - 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        d = case (safeGet (fst p_cut + 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        l = case (safeGet (fst p_cut) (snd p_cut - 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        r = case (safeGet (fst p_cut) (snd p_cut + 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1    
    
    check_route_left :: Point1 -> Point1 -> Point1 -> Point1 -> Maze -> Maze
    check_route_left p1 p2 p_n p_cut m
      | (l + r + u + d > 2) =  
          check_route_left p1 p2 p_n (fst p_cut, (snd p_cut) - 1) m

      | otherwise = setElem route p_cut m
      where
        u = case (safeGet (fst p_cut - 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        d = case (safeGet (fst p_cut + 1) (snd p_cut) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        l = case (safeGet (fst p_cut) (snd p_cut - 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1
        r = case (safeGet (fst p_cut) (snd p_cut + 1) m) of 
          Just 1 -> 1
          Just 0 -> 0
          Nothing -> 1  
  
setW :: Point1 -> Point1 -> Point1 -> Point1 -> AppState -> Maze
setW p1 p2 p_n set_point (AppState m _ _ _ rand) 
  | ((fst set_point) > (fst p2)) = m
  | otherwise = let
    m1 = setElem wall set_point m
    in setW p1 p2 p_n ((fst set_point) + 1, snd set_point) (AppState m1 p_in False False rand)
  
setH :: Point1 -> Point1 -> Point1 -> Point1 -> AppState -> Maze
setH p1 p2 p_n set_point (AppState m _ _ _ rand)
  | ((snd set_point) > (snd p2)) = m
  | otherwise = let
    m1 = setElem wall set_point m
    in setH p1 p2 p_n (fst set_point, (snd set_point) + 1) (AppState m1 p_in False False rand)
      
------------------------------------------------------------------------
--Other finctions.
------------------------------------------------------------------------




run :: IO()
run = do
  size <- System.Environment.getArgs
  if (size <= 2) then putStdLn("Size of maze must be more than 2")
  else do
    rndGen <- newStdGen  

    play display bgColor fps (AppState (zero size size) p_in False False rndGen)
        drawApp handleEvent updateApp
  