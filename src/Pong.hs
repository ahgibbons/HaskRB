{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Pong (pongMain) where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

data PongGame = Game
         { ballLoc :: (Float, Float)
         , ballVel :: (Float, Float)
         , player1 :: Float
         , player2 :: Float
         , paused :: Bool} deriving (Show)

type Radius = Float
type Position = (Float, Float)

render :: PongGame -> Picture
render game =
    pictures [ball, walls,
             mkPaddle rose 120 $ player1 game,
             mkPaddle orange (-120) $ player2 game]
  where
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid 10
    ballColor = dark red
    wall :: Float -> Picture
    wall offset =
        translate 0 offset $
          color wallColor $
             rectangleSolid 270 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
            [ translate x y $ color col $ rectangleSolid 26 86
            , translate x y $ color paddleColor $ rectangleSolid 20 80]
    paddleColor = light (light blue)
          
initialState :: PongGame
initialState = Game
         { ballLoc = (-10, 30)
         , ballVel = (0, -50)
         , player1 = 40
         , player2 = (-80)
         , paused = False}

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y')}
    where
      (x,y) = ballLoc game
      (vx, vy) = ballVel game

      x' = x + vx * seconds
      y' = y + vy * seconds

--paddleBounce :: PongGame -> PongGame

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel  = (vx,vy')}
  where
    radius = 10
    (vx, vy) = ballVel game
    vy' = if wallCollision (ballLoc game) radius
          then (-vy)
          else vy
              
wallCollision :: Position -> Radius -> Bool
wallCollision (_,y) radius = topCollision || bottomCollision
    where
      topCollision    = y - radius <= (-1)* fromIntegral width / 2
      bottomCollision = y + radius >= fromIntegral width / 2

movePaddleL :: Float -> PongGame -> PongGame
movePaddleL dy game = game {player2 = y' + dy}
    where y' = player2 game

movePaddleR :: Float -> PongGame -> PongGame
movePaddleR dy game = game {player1 = y' + dy}
    where y' = player1 game
                        
handleKeys :: Event -> PongGame -> PongGame
handleKeys (EventKey (Char 'r') _ _ _) game = game { ballLoc = (0,0)}
handleKeys (EventKey (Char 'p') _ _ _) game = let p' = paused game
                                              in game { paused = not p'}
handleKeys (EventKey (Char 'w') _ _ _) game = movePaddleL 10 game
handleKeys (EventKey (Char 's') _ _ _) game = movePaddleL (-10) game
handleKeys (EventKey (SpecialKey KeyUp) _ _ _) game = movePaddleR 10 game
handleKeys (EventKey (SpecialKey KeyDown) _ _ _) game = movePaddleR (-10) game
                                                 
handleKeys _ game = game
    
width, height, offset :: Int
width = 300
height = 300
offset = 100

fps :: Int
fps  = 60

window :: Display
window = InWindow "Pong" (width, height) (offset,offset)

background :: Color
background = black


update :: Float -> PongGame -> PongGame
update seconds game = if pause then game
                               else wallBounce . moveBall seconds $ game
    where
      pause = paused game
                  
pongMain :: IO ()
pongMain = play window background fps initialState render handleKeys update
