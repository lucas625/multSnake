module Eventos where
    -- import Data
    import Control.Concurrent.Mvar
    import Graphics.Gloss.Interface.Pure.Game

    eventos :: Event -> (Snakegame, Control, Control) -> IO(Snakegame, Control, Control)
    -- pressionar tecla
    -- Player 1

    -- cima p1
    eventos (SpecialKey KeyUp) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        if(a) == 0
        then do
            putMVar p1Control (a+1)
            vel = snakeVel game
            return (game { p1 = (Obj x y 0 vel) } , p1Control, p2Control)
            where (Obj x y vx vy) = p1 game
        else do
            putMVar p1Control a

    -- baixo p1
    eventos (SpecialKey KeyDown) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        if(a) == 0
            then do
                putMVar p1Control (a+1)
                vel = snakeVel game
                return (game { p1 = (Obj x y 0 -vel) } , p1Control, p2Control)
                where (Obj x y vx vy) = p1 game
            else do
                putMVar p1Control a

    -- esquerda p1
    eventos (SpecialKey KeyLeft) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        if(a) == 0
            then do
                vel = snakeVel game
                putMVar p1Control (a+1)
                return (game { p1 = (Obj x y -vel 0) } , p1Control, p2Control)
                where (Obj x y vx vy) = p1 game
            else do
                putMVar p1Control a

    -- direita p1
    eventos (SpecialKey KeyRight) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        if(a) == 0
            then do
                vel = snakeVel game
                putMVar p1Control (a+1)
                return (game { p1 = (Obj x y vel 0) } , p1Control, p2Control)
                where (Obj x y vx vy) = p1 game
            else do
                putMVar p1Control a

    -- Player 2

    -- cima p2
    eventos (Char 'w') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        if(a) == 0
            then do
                vel = snakeVel game
                putMVar p2Control (a+1)
                return (game { p2 = (Obj x y 0 vel) } , p1Control, p2Control)
                where (Obj x y vx vy) = p2 game
            else do
                putMVar p2Control a

    -- baixo p2
    eventos (Char 's') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        if(a) == 0
            then do
                vel = snakeVel game
                putMVar p2Control (a+1)
                return (game { p2 = (Obj x y 0 -vel) } , p1Control, p2Control)
                where (Obj x y vx vy) = p2 game
            else do
                putMVar p2Control a

    -- esquerda p2
    eventos (Char 'a') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        if(a) == 0
            then do
                vel = snakeVel game
                putMVar p2Control (a+1)
                return (game { p2 = (Obj x y -vel 0) } , p1Control, p2Control)
                where (Obj x y vx vy) = p2 game
            else do
                putMVar p2Control a

    -- direita p2
    eventos (Char 'd') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        if(a) == 0
            then do
                vel = snakeVel game
                putMVar p2Control (a+1)
                return (game { p2 = (Obj x y vel 0) } , p1Control, p2Control)
                where (Obj x y vx vy) = p2 game
            else do
                putMVar p2Control a
    -- mudar nível
    eventos (EventKey (SpecialKey KeySpace) _ _ _) (game, p1Control, p2Control) = do
        if (gameType==1)--nível 1
            then do
                return (levelTwoState, p1Control, p2Control)
            else if (gameType==2)--nível 2
                then do
                    return (levelThreeState, p1Control, p2Control)
            else if (gameType==3)--nível 3
                then do
                    return (levelOneState, p1Control, p2Control)--de volta ao nível 1
            else do
                return (levelOneState, p1Control, p2Control)
    
    -- KEYUP events

    cancelp1 = do
        c <- takeMVar p1Control
        putMVar p1Control 0
        return( game { p1 = (Obj x1 y1 0 0) }, p1Control, p2Control )
    
    cancelp2 = do
        c <- takeMVar p2Control
        putMVar p2Control 0
        return( game { p2 = (Obj x1 y1 0 0) }, p1Control, p2Control )
    
    eventos (EventKey k (Up) _ _) (game, p1Control, p2Control)
        -- Player 1
        | (SpecialKey KeyUp) <- k = do
            return cancelp1

        | (SpecialKey KeyDown) <- k = do
            return cancelp1

        | (SpecialKey KeyLeft) <- k = do
            return cancelp1
        
        | (SpecialKey KeyDown) <- k = do
            return cancelp1

        -- Player 2
        | (Char 'w') <- k = do
            return cancelp2

        | (Char 's') <- k = do
            return cancelp2

        | (Char 'a') <- k = do
            return cancelp2

        | (Char 'd') <- k = do
            return cancelp2
            
        | otherwise = do -- nothing
            return(game, p1Control, p2Control)
            where
                (Obj x1 y1 vx1 vy1) = p1 game
                (Obj x2 y2 vx2 vy2) = p2 game

    -- default
    eventos _ (game, p1Control, p2Control) = return (game, p1Control, p2Control)