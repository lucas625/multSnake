module Fruta where

    import Data
    import Cobra
    import Graphics.Gloss.Interface.Pure.Game

    mkFruta :: Float -> Float -> Picture
    mkFruta x y = uncurry translate trans $ color ballColor $ circleSolid ballRadius
        where 
            trans = (x, y)

    snakeCollision :: Float -> Float -> Position -> Float -> Float -> Float -> Float -> Int
    snakeCollision snakeW snakeH (x, y) x1 y1 x2 y2 = val
        where
            collisionP1 = (x+ballRadius+halfw >= x1) && (x-ballRadius-halfw <= x1) && (y+ballRadius+halfh >= y1) && (y-ballRadius-halfh <= y1)
            collisionP2 = (x+ballRadius+halfw >= x2) && (x-ballRadius-halfw <= x2) && (y+ballRadius+halfh >= y2) && (y-ballRadius-halfh <= y2)
            halfw = snakeW / 2
            halfh = snakeH / 2
            val = if collisionP1 then 1
                else if collisionP2 then 2
                else 0
    
    snakeGo :: SnakeGame -> SnakeGame
    snakeGo game = game { fruta = (Obj x1' y1' vx1' vy1')
                            , score = (p1Score',p2Score')
                            , result = result'
                            , isOver = isOver' }
        where
                (Obj bx1 by1 bvx1 bvy1) = fruta game
                (Obj x1 y1 _ _) = p1 game
                (Obj x2 y2 _ _) = p2 game
                (p1Score, p2Score) = score game  
                
                pointx = snakeCollision cobraWidth cobraHeight (bx1, by1) x1 y1 x2 y2

                p1Score' = if isOver game
                    then p1Score
                    else if pointx == 1
                    then (p1Score + 1)
                    else p1Score
                
                p2Score' = if isOver game
                    then p2Score
                    else if pointx == 2
                    then (p2Score + 1)
                    else p2Score

                result' = if p1Score' == maxScore
                        then 1
                        else if p2Score' == maxScore
                        then 2
                        else 0
                
                isOver' = if result' /= 0
                        then True
                        else False

                x1' = if pointx == 0
                    then bx1
                    else fromIntegral (rem (floor(by1+120)) (floor((fromIntegral width)/2)))
                    
                y1' = if pointx == 0
                    then by1
                    else fromIntegral (rem (floor(by1-150)) (floor((fromIntegral height)/2)))
                
                vx1' = 0
                vy1' = 0


    

