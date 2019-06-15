module Fruta where

    import Data
    import Snake
    import Graphics.Gloss.Interface.Pure.Game

    mkBall :: Float -> Float -> Picture
    mkBall x y = uncurry translate trans $ color ballColor $ circleSolid ballRadius
        where 
            trans = (x, y)

          
    point :: Float -> Int
    point x = 
        if x > 275 
        then 1
        else if x < (-275)
        then 2
        else 0

    randomPoint :: Int -> Int
    randomPoint x = randomR (50,50)

    eatFruit :: Snake ->