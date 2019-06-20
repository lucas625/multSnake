module Cobra where

    import Data
    import Graphics.Gloss.Interface.Pure.Game

    mkcobra :: Float -> Float -> Float -> Float -> Color -> Picture
    mkcobra x y cobraWidth cobraHeight cobracor= pictures
        [ translate x y $ color cobracor $ rectangleSolid cobraWidth cobraHeight]


    moveCobra :: Float -> Int -> Int -> SnakeGame -> SnakeGame
    moveCobra seconds width height game = game 
        { p1 = (Obj x1' y1' vx1 vy1)
        , p2 = (Obj x2' y2' vx2 vy2) 
        } where 
            -- Old values
            (Obj x1 y1 vx1 vy1) = p1 game
            (Obj x2 y2 vx2 vy2) = p2 game
            -- New Values
            x1' = if (x1 + vx1 * seconds) >= (convIF width)/2 then (-(convIF width)/2)
                else if (x1 + vx1 * seconds) <= (-(convIF width)/2) then ((convIF width)/2)
                else x1 + vx1 * seconds

            y1' = if (y1 + vy1 * seconds) >= (convIF height)/2 then (-(convIF height)/2)
                else if (y1 + vy1 * seconds) <= (-(convIF height)/2) then ((convIF height)/2)
                else y1 + vy1 * seconds

            x2' = if (x2 + vx2 * seconds) >= (convIF width)/2 then (-(convIF width)/2)
                else if (x2 + vx2 * seconds) <= (-(convIF width)/2) then ((convIF width)/2)
                else x2 + vx2 * seconds
            
            y2' = if (y2 + vy2 * seconds) >= (convIF height)/2 then (-(convIF height)/2)
                else if (y2 + vy2 * seconds) <= (-(convIF height)/2) then ((convIF height)/2)
                else y2 + vy2 * seconds
            
    convIF :: Int -> Float
    convIF numero = fromIntegral numero