module Data where
    import Control.Concurrent.MVar
    import Graphics.Gloss.Interface.Pure.Game
    import System.Random

    width, height:: Int
    width = 600
    height = 600

    maxScore :: Integer
    maxScore = 15

    cobraWidth,ballRadius,cobraHeight :: Float
    cobraWidth = 20
    ballRadius = 10
    cobraHeight = 20
    
    ballColor :: Color
    ballColor = green

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
        , fruta = (Obj 0 0 0 0)
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
        , fruta = (Obj 0 0 0 0)
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
        , fruta = (Obj 0 0 0 0)
        }

