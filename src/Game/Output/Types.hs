module Game.Output.Types where

import Data.Map

import qualified SDL
import qualified SDL.Mixer as Mix

type GraphicWindowSize = (Int, Int)

data GraphicsEnv = GraphicsEnv { gWindowSize :: GraphicWindowSize
                               , gWindow     :: SDL.Window
                               , gRenderer   :: SDL.Renderer
                               , gImages     :: Map String GraphicImage
                               }

type GraphicImage = (SDL.Texture, SDL.Surface)

data AudioEnv = AudioEnv { sMusic :: Mix.Music
                         }
