module Score where

    import Data
    import Graphics.Gloss.Interface.Pure.Game

    mkScore :: Integer -> Integer -> Integer -> Picture
    mkScore p1Score p2Score result = scale 0.25 0.25 (translate (-1000) (600) $ Color white $ Text board)
        where 
            board = if result == 0
                    then (show p1Score ++ " - " ++ show p2Score) 
                    else if result == 1
                    then ("p1 ganhou!")
                    else ("p2 ganhou!")