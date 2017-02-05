{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Pong (pongMain) where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.Pure.Game
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
         , player2 :: Float } deriving (Show)

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
         , ballVel = (1, -3)
         , player1 = 40
         , player2 = (-80)}

moveBall :: Float -> PongGame -> PongGame
moveBall seconds game = game { ballLoc = (x', y')}
    where
      (x,y) = ballLoc game
      (vx, vy) = ballVel game

      x' = x + vx * seconds
      y' = y + vy * seconds

    
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

drawing :: Picture
drawing = pictures [ball, walls,
                   mkPaddle rose 120 (-20),
                   mkPaddle orange (-120) 40]
  where
    ball = translate (-10) 40 $ color ballColor $ circleSolid 10
    ballColor = dark red

    wall :: Float -> Picture
    wall offset = translate 0 offset $
                    color wallColor $
                       rectangleSolid 270 10

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]

    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures
        [ translate x y $ color col $ rectangleSolid 26 86
        , translate x y $ color paddleColor $ rectangleSolid 20 80]

    paddleColor = light (light blue)

update :: ViewPort -> Float -> PongGame -> PongGame
update  _ = moveBall
                  
pongMain :: IO ()
pongMain = simulate window background fps initialState render update
     where
       frame :: Float -> Picture
       frame seconds = render $ moveBall seconds initialState
