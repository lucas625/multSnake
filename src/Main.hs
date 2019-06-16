import Control.Concurrent.MVar

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Interface.Pure.Game as G

import Data
import Eventos
import Fruta
import Cobra


    
background :: Color
background = red

window :: Display
window = InWindow "Nice Window" (500, 500) (10, 10)


drawing :: Picture
drawing = circle 80



main = do
    display window background drawing