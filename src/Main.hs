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
import Score


    
background :: Color
background = black

window :: Display
window = InWindow "Nice Window" (500, 500) (10, 10)


drawing :: Picture
drawing = circle 80

fps :: Int
fps = 60

render :: (SnakeGame, Control, Control) -> IO Picture
render (game, _, _) = do
    return(pics)    
    where 
        pics = pictures [ mkcobra x1 y1 tamcobra
                        , mkcobra x2 y2 tamcobra
                        ]
        (Obj bx1 by1 _ _) = fruta game
        (Obj x1 y1 _ _) = p1 game
        (Obj x2 y2 _ _) = p2 game
        padH = padHeight game


update :: Float -> (SnakeGame, Control, Control) -> IO (SnakeGame, Control, Control)
update seconds (game, p1Control, p2Control) = return ( (moveCobra seconds game), p1Control, p2Control )


main = do
    p1Control <- newMVar 0
    p2Control <- newMVar 0
    playIO window background fps (levelOneState, p1Control, p2Control) render eventos update


