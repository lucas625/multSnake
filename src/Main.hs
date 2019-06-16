import Control.Concurrent.MVar

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Data
import Eventos
import Score

window :: Display
window = InWindow "multSnake" (width, height) (leftOffset, upOffset)

background :: Color
background = makeColor 0.1 0.6 0.2 0.6

-- Frames per Second
fps :: Int
fps = 60

main :: IO()
main = do 
    p1Control <- newMVar 0
    p2Control <- newMVar 0
    print(50)
    --playIO window background fps (levelOneState, p1Control, p2Control) render events update