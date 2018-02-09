{-# LANGUAGE GADTs #-}
module Game.Output.Audio
    ( startMusic
    , stopMusic
    ) where

import Game.Output.Types

import qualified SDL.Mixer as Mix

startMusic :: AudioEnv -> IO ()
startMusic env@(AudioEnv music) = Mix.playMusic Mix.Forever music

stopMusic :: IO ()
stopMusic = Mix.haltMusic
