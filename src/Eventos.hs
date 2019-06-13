module Eventos where
    -- import Data
    import Control.Concurrent.Mvar
    import Graphics.Gloss.Interface.Pure.Game

    eventos :: Evento -> (Snakegame, Control, Control) -> IO(Snakegame, Control, Control)
    -- pressionar tecla
    -- Player 1

    -- cima p1
    eventos (SpecialKey KeyUp) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y 0 maxSnakeVel) } , p1Control, p2Control)
        where (Obj x y vx vy) = p1 game

    -- baixo p1
    eventos (SpecialKey KeyDown) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y 0 -maxSnakeVel) } , p1Control, p2Control)
        where (Obj x y vx vy) = p1 game

    -- esquerda p1
    eventos (SpecialKey KeyLeft) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y -maxSnakeVel 0) } , p1Control, p2Control)
        where (Obj x y vx vy) = p1 game

    -- direita p1
    eventos (SpecialKey KeyRight) (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p1Control
        putMVar p1Control (a+1)
        return (game { p1 = (Obj x y maxSnakeVel 0) } , p1Control, p2Control)
        where (Obj x y vx vy) = p1 game

    -- Player 2

    -- cima p2
    eventos (Char 'w') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control (a+1)
        return (game { p2 = (Obj x y 0 maxSnakeVel) } , p1Control, p2Control)
        where (Obj x y vx vy) = p2 game

    -- baixo p2
    eventos (Char 's') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control (a+1)
        return (game { p2 = (Obj x y 0 -maxSnakeVel) } , p1Control, p2Control)
        where (Obj x y vx vy) = p2 game

    -- esquerda p2
    eventos (Char 'a') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control (a+1)
        return (game { p2 = (Obj x y -maxSnakeVel 0) } , p1Control, p2Control)
        where (Obj x y vx vy) = p2 game

    -- direita p2
    eventos (Char 'a') (Down) _ _) (game, p1Control, p2Control) = do
        a <- takeMVar p2Control
        putMVar p2Control (a+1)
        return (game { p2 = (Obj x y maxSnakeVel 0) } , p1Control, p2Control)
        where (Obj x y vx vy) = p2 game

    