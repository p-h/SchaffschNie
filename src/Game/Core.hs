module Game.Core where

import Control.Monad
import Control.Concurrent

import qualified FRP.Yampa as Yampa
import FRP.Yampa ( Event(..)
                 , DTime
                 , SF
                 )

import Game.AppTypes
import Game.Types

import qualified Game.Input.Core as Input
import qualified Game.Output.Core as Output

import qualified Game.Level.Reader as Level
import qualified Game.Process.Core as Process


run :: IO ()
run = do
        graficsEnv <- Output.init (300,300) "Test"

        lvl <- Level.read 1
        let gameData = GameData [lvl] initGameSession

        startYampa (Event <$> Input.input)
                   (Output.output graficsEnv)
                   Input.getTime
                   (Process.run gameData)

        Output.quit graficsEnv


startYampa :: IO AppInputEvent                  -- input function
            -> (Output.RenderObject -> IO ())   -- output function
            -> IO Double                        -- time function
            -> SF AppInputEvent AppOutput       -- process function
            -> IO ()
startYampa inputFunction outputFunction timeFunction processFunction = do
        timeMVar <- newMVar =<< timeFunction

        let
            yampaInitial :: IO AppInputEvent
            yampaInitial = return NoEvent

            yampaInput :: Bool -> IO (DTime, Maybe AppInputEvent)
            yampaInput _canBlock = do
                    deltaTime <- getTimeDelta timeMVar =<< timeFunction
                    appInputEvent <- inputFunction
                    return (deltaTime, Just appInputEvent)
                    where
                        getTimeDelta :: Fractional a => MVar a -> a -> IO a
                        getTimeDelta mVar currentTime = (currentTime -) <$> swapMVar mVar currentTime

            yampaOutput :: Bool -> AppOutput -> IO Bool
            yampaOutput changed appOutput = do
                    when changed $ outputFunction $ outRenderObject appOutput
                    return $ outExit appOutput

        Yampa.reactimate yampaInitial yampaInput yampaOutput processFunction
