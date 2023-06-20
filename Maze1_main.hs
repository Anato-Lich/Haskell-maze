module Maze1 where

import Graphics.Gloss.Interface.Pure.Game
import System.Random
import System.Environment
import Data.Matrix
import Debug.Trace


------------------------------------------------------------------------
-- Constants.
------------------------------------------------------------------------
size :: Int
size = 10

route :: Int
route = 0

wall :: Int
wall = 1


-- Random numbers range.
numbersRange :: (Int, Int)
numbersRange = (1, size)

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
p_in = (0, 1)

p_out :: Point1
p_out = (size, size)

type Maze = Matrix Int

data AppState = AppState
  {
    maze :: Maze, --Matrix for maze
    user_state :: (Int, Int), -- User state.
    maze_state :: Bool, -- Maze state: False - in work, True - finished
    end :: Bool  -- End flag: False - user in maze, True - in end point
  }

------------------------------------------------------------------------
--Events for moving
------------------------------------------------------------------------
handleEvent :: Event -> AppState -> AppState

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) 
  (AppState m user True False) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False
    Just k -> if k == 1 then AppState m user True False
    else AppState m new_u True False  
  
    where
      new_u = (fst user - 1, snd user)

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) 
  (AppState m user True False) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False
    Just k -> if k == 1 then AppState m user True False
    else AppState m new_u True False  
  
    where
      new_u = (fst user + 1, snd user)

handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) 
  (AppState m user True False) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False
    Just 1 -> AppState m user True False
    Just 0 -> AppState m new_u True False  
  
    where
      new_u = (fst user, snd user - 1)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) 
  (AppState m user True False) = 
  case (safeGet (fst new_u) (snd new_u) m) of
    Nothing -> AppState m user True False
    Just k -> if k == 1 then AppState m user True False
    else AppState m new_u True False  
  
    where
      new_u = (fst user, snd user + 1)

-- Ignore all other events.
handleEvent _ (state) = state

------------------------------------------------------------------------

-- Simulation step (updates nothing).
updateApp :: Float -> AppState -> AppState
updateApp _ x = x

-- Draw a picture: maze and user position.
drawApp :: AppState -> Picture
drawApp (AppState m _ False _) = Pictures squares
  where 
    squares = [checkWall m i j | i <- [1..size], j <- [1..size]]
    checkWall :: Maze -> Int -> Int -> Picture
    checkWall m i j = 
      case (safeGet i j m) of 
        Just 1 -> Color black $ pic 
        Just 0 -> Color white $ pic
        Nothing -> error "Error at drawApp"
      where 
        pic = Polygon [(a + 30*i1,b + 30*j1), (a+30*i1+30, b+30*j1), 
          (a+30*i1+30, b+30*j1+30), (a+30*i1, b+30*j1+30)]
        a = fromIntegral i :: Float
        b = fromIntegral j :: Float
        i1 = fromIntegral i :: Float
        j1 = fromIntegral j :: Float
        shift1 = fromIntegral (30 * j) :: Float
        shift2 = fromIntegral (-30 * i) :: Float
        
drawApp (AppState m user True False) = Debug.Trace.trace ("user") pic1
  where
    pic1 = Color green $ Translate shift1 shift2 u_pic 
    u_pic = Polygon [(a,b), (a+8, b), (a+8, b+8), (a, b+8)]
    a = fromIntegral (fst user) :: Float
    b = fromIntegral (snd user) :: Float
    shift1 = fromIntegral (fst user) :: Float
    shift2 = fromIntegral (snd user) :: Float

-- Main function for generation
-- лабиринт без стен в матрице
-- внешние стены не учитываются - отрисовать в run do where?

createMaze :: Point1 -> Point1 -> AppState -> StdGen -> Maze

createMaze _ _ (AppState m _ True _) _ = Debug.Trace.trace ("End" ++ show m) m -- Finish

createMaze p1 p2 (AppState m _ False _) rnd
  | ((fst p2 - fst p1) < 2) || ((snd p2 - snd p1) < 2) = 
    createMaze p1 p2 (AppState m p_in True False) rnd
    
  | otherwise = let -- так вообще можно?
      mW = setW p1 p2 p_n (fst p1, snd p_n) state
      mH = setH p1 p2 p_n (fst p_n, snd p1)( state {maze = mW} )
      m1 = createMaze p1 (fst p_n - 1, snd p_n - 1) 
        (state {maze = mH}) newrnd1
      m2 = createMaze (fst p1, snd p_n + 1) (fst p_n - 1, snd p2) 
        (state {maze = m1} ) newrnd1
      m3 = createMaze (fst p_n + 1, snd p1) (fst p2, snd p_n - 1) 
        (state {maze = m2} ) newrnd1
      m4 = createMaze (fst p_n + 1, snd p_n + 1) p2 
        (state {maze = m3 } ) newrnd1
      in createMaze p1 p2 (state {maze = m4, maze_state = True}) newrnd1
      where 
        (newx, newrnd) = randomR (fst p1, fst p2) rnd
        (newy, newrnd1) = randomR (snd p1, snd p2) newrnd
        p_n = (newx, newy)
        state = (AppState m p_in False False)
  
  
  
setW :: Point1 -> Point1 -> Point1 -> Point1 -> AppState -> Maze
setW p1 p2 p_n set_point (AppState m _ _ _) 
  | ((fst set_point) > (fst p2)) = m
  | otherwise = let
    m1 = Debug.Trace.trace (show m ++ show p1 ++ show p2 ++ show p_n) (setElem wall set_point m)
    in setW p1 p2 p_n ((fst set_point) + 1, snd set_point) (AppState m1 p_in False False)
  
setH :: Point1 -> Point1 -> Point1 -> Point1 -> AppState -> Maze
setH p1 p2 p_n set_point (AppState m _ _ _)
  | ((snd set_point) > (snd p2)) = m
  | otherwise = let
    m1 = setElem wall set_point m
    in setH p1 p2 p_n (fst set_point, (snd set_point) + 1) (AppState m1 p_in False False)
      
------------------------------------------------------------------------
--Other finctions.
------------------------------------------------------------------------




run :: IO()
run = do
  rndGen <- newStdGen  

  play display bgColor fps (AppState (createMaze (1,1) (size, size) 
    (AppState (zero size size) p_in False False) rndGen) p_in False False)
      drawApp handleEvent updateApp
  