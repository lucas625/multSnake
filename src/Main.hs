import Control.Concurrent.MVar

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game

import Data
import Eventos

main = do
    display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)