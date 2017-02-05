{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Game (glMain) where

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

--import Reactive.Banana as R
--import Reactive.Banana.Frameworks as R

data GameWorld = GameWorld {charPic :: Picture, charX :: Int}
    
glMain :: IO ()
glMain = do
  character <- loadBMP "link_1.bmp"
  let world = initWorld character
--  animate (InWindow "Nice Animation" (200,200) (10,10)) white circlePic
--  display (InWindow "Nice Window" (200,200) (10,10)) white (character)
  play (InWindow "Game" (400,400) (10,10)) white 10 world stepWorld eventHandler worldIter

initWorld :: Picture -> GameWorld
initWorld player = GameWorld {charPic=player, charX=10}

stepWorld :: GameWorld -> Picture
stepWorld g = translate (fromIntegral x) 0 (charPic g)
  where x = charX g
              
eventHandler :: Event -> GameWorld -> GameWorld
eventHandler (EventKey (Char 's') _ _ _) g = let x' = charX g
                                             in g {charX=x'+10}
eventHandler _ g = g

--worldIter :: Float -> GameWorld -> GameWorld
worldIter _ g = g

              
circlePic :: Float -> Picture
circlePic t = Pictures [Circle (radius t), color red (circleSolid (20))]

radius :: Float -> Float
radius t = 80*(cos t)
