{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Slots where

import Graphics.Gloss
import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Data.List (nub)
import System.Random
import System.IO
import Debug.Trace
import Data.IORef

import Reactive.Banana as R
import Reactive.Banana.Frameworks as R

glMain :: IO ()
glMain = do
    displayHelpMessage
    sources <- makeSources
    network <- compile $ networkDescription sources
    actuate network
    eventLoop sources

displayHelpMessage :: IO ()
displayHelpMessage = mapM_ putStrLn $
    "-----------------------------":
    "- THE REACTIVE SLOT MACHINE -":
    "------ WIN A BANANA ---------":
    "":
    "Commands are:":
    "   coin    - insert a coin":
    "   play    - play one game":
    "   quit    - quit the program":
    "":[]

makeSources = (,) <$> newAddHandler <*> newAddHandler

eventLoop :: (EventSource (), EventSource ()) -> IO ()
eventLoop (escoin,esplay) = loop
    where
      loop = do
        putStr "> "
        hFlush stdout
        s <- getLine
        case s of
            "coin" -> fire escoin ()
            "play" -> fire esplay ()
            "quit" -> return ()
            _      -> putStrLn $ s ++ " - unknown command"
        when (s /= "quit") loop


type EventSource a = (AddHandler a, a -> IO ())

addHandler :: EventSource a -> AddHandler a
addHandler = fst

fire :: EventSource a -> a -> IO ()
fire = snd


type Money = Int
type Reels = (Int,Int,Int)
data Win = Double | Triple


networkDescription :: (EventSource (), EventSource ()) -> MomentIO ()
networkDescription (escoin,esplay) = mdo

    initialStdGen <- liftIO $ newStdGen

    ecoin <- fromAddHandler (addHandler escoin)
    eplay <- fromAddHandler (addHandler esplay)

    (ecredits :: Event Money, bcredits :: Behavior Money)
      <- mapAccum 0 . fmap (\f x -> (f x, f x)) $ unions $
           [ addCredit    <$ ecoin
           , removeCredit <$ edoesplay
           , addWin       <$> ewin
           ]

    let
        addCredit     = (+1)
        removeCredit  = subtract 1
        addWin Double = (+5)
        addWin Triple = (+20)

        emayplay :: Event Bool
        emayplay = (\credits _ -> credits > 0) <$> bcredits <@> eplay

        edoesplay :: Event ()
        edoesplay = () <$ filterE id emayplay

        edenied :: Event ()
        edenied = () <$ filterE not emayplay

    (eroll :: Event Reels, bstdgen :: Behavior StdGen)
      <- mapAccum initialStdGen $ roll <$> edoesplay

    let
        roll :: () -> StdGen -> (Reels,StdGen)
        roll () gen0 = ((z1,z2,z3),gen3)
             where
               random    = randomR(1,4)
               (z1,gen1) = random gen0
               (z2,gen2) = random gen1
               (z3,gen3) = random gen2

        ewin :: Event Win
        ewin = fmap fromJust $ filterE isJust $ fmap checkWin eroll
        checkWin (z1,z2,z3)
            | length (nub [z1,z2,z3]) == 1 = Just Triple
            | length (nub [z1,z2,z3]) == 2 = Just Double
            | otherwise                    = Nothing

    reactimate $ putStrLn . showCredit <$> ecredits
    reactimate $ putStrLn . showRoll   <$> eroll
    reactimate $ putStrLn . showWin    <$> ewin
    reactimate $ putStrLn "Not enough credits!" <$ edenied

showCredit money     = "Credits: " ++ show money
showRoll (z1,z2,z3)  = "You rolled " ++ show z1 ++ show z2 ++ show z3
showWin Double = "Wow, a double!"
showWin Triple = "Wowowow! A triple! So awesome!"
