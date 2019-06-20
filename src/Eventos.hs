module Eventos where
    
    import Data
    import Control.Concurrent.MVar
    import Graphics.Gloss.Interface.Pure.Game

    eventos :: Event -> (SnakeGame, Control, Control) -> IO(SnakeGame, Control, Control)
    -- pressionar tecla
    -- Player 1

    -- cima p1
    eventos (EventKey (SpecialKey KeyUp) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y 0 vel) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p1 game
            vel = snakeVel game

    -- baixo p1
    eventos (EventKey (SpecialKey KeyDown) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y 0 (-vel)) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p1 game
            vel = snakeVel game

    -- esquerda p1
    eventos (EventKey (SpecialKey KeyLeft) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y (-vel) 0) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p1 game
            vel = snakeVel game

    -- direita p1
    eventos (EventKey (SpecialKey KeyRight) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y vel 0) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p1 game
            vel = snakeVel game

    -- Player 2

    -- cima p2
    eventos (EventKey (Char 'w') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control 1
        return (game { p2 = (Obj x y 0 vel) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p2 game
            vel = snakeVel game

    -- baixo p2
    eventos (EventKey (Char 's') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control 1
        return (game { p2 = (Obj x y 0 (-vel)) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p2 game
            vel = snakeVel game

    -- esquerda p2
    eventos (EventKey (Char 'a') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control 1
        return (game { p2 = (Obj x y (-vel) 0) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p2 game
            vel = snakeVel game

    -- direita p2
    eventos (EventKey (Char 'd') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control 1
        return (game { p2 = (Obj x y vel 0) } , p1Control, p2Control)
        where 
            (Obj x y vx vy) = p2 game
            vel = snakeVel game

    -- mudar nível
    eventos (EventKey (SpecialKey KeySpace) _ _ _) (game, p1Control, p2Control) = do
        if (gt==1)--nível 1
            then do
                return (levelTwoState, p1Control, p2Control)
            else if (gt==2)--nível 2
                then do
                    return (levelThreeState, p1Control, p2Control)
            else if (gt==3)--nível 3
                then do
                    return (levelOneState, p1Control, p2Control)--de volta ao nível 1
            else do
                return (levelOneState, p1Control, p2Control)
            where gt = gameType game
    
    -- default
    eventos _ (game, p1Control, p2Control) = return (game, p1Control, p2Control)