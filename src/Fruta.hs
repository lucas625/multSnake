module Fruta where

    import Data
    import Cobra
    import Graphics.Gloss.Interface.Pure.Game

    mkFruta :: Float -> Float -> Picture
    mkFruta x y = uncurry translate trans $ color ballColor $ circleSolid ballRadius
        where 
            trans = (x, y)

          
    point :: Float -> Int
    point x = 
        if x > 275 
        then 1
        else if x < (-275)
        then 2
        else 0


    

