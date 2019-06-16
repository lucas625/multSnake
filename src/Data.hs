module Data where
    import Control.Concurrent.MVar
    import Graphics.Gloss.Interface.Pure.Game

    maxScore :: Integer
    maxScore = 15

    padWidth, padHeight, ballRadius :: Float
    padWidth = 20
    padHeight = 20
    ballRadius = 10
    
    padColor,ballColor :: Color
    padColor = light blue
    ballColor = red

    data Object = Obj 
        { x :: Float, y :: Float
        , vx :: Float, vy :: Float 
        } deriving Show

    data SnakeGame = Game
        { p1 :: Object
        , p2 :: Object
        , score :: (Integer, Integer)
        , result :: Integer
        , isOver :: Bool
        , snakeVel :: Float
        , gameType :: Integer
        , fruta :: Object
        } deriving Show 

    type Radius = Float 
    type Position = (Float, Float) 
    type Control = MVar Integer

    levelOneState :: SnakeGame
    levelOneState = Game 
        { p1 = (Obj (-260) 0 0 0)
        , p2 = (Obj 260 0 0 0)
        , score = (0, 0)
        , result = 0
        , isOver = False
        , snakeVel = 450
        , gameType = 1
        }

    levelTwoState :: SnakeGame
    levelTwoState = Game 
        { p1 = (Obj (-260) 0 0 0)
        , p2 = (Obj 260 0 0 0)
        , score = (0, 0)
        , result = 0
        , isOver = False
        , snakeVel = 600
        , gameType = 2
        }

    levelThreeState :: SnakeGame
    levelThreeState = Game 
        { p1 = (Obj (-260) 0 0 0)
        , p2 = (Obj 260 0 0 0)
        , score = (0, 0)
        , result = 0
        , isOver = False
        , snakeVel = 800
        , gameType = 3
        }
