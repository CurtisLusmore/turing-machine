import Turing

reprStep (q, as, bs) = (reverse $ take 5 as) ++ take 5 bs

steps1 = map reprStep $ run ' ' [] [] 1 step []
  where
    step (1, _) = (2, '0', MoveRight)
    step (2, _) = (3, ' ', MoveRight)
    step (3, _) = (4, '1', MoveRight)
    step (4, _) = (1, ' ', MoveRight)

steps2 = map reprStep $ run '0' [] ['1', '1'] 1 step [0]
  where
    step (1, '0') = (0, '0', Stay)
    step (1, '1') = (2, '0', MoveRight)
    step (2, '0') = (3, '0', MoveRight)
    step (2, '1') = (2, '1', MoveRight)
    step (3, '0') = (4, '1', MoveLeft)
    step (3, '1') = (3, '1', MoveRight)
    step (4, '0') = (5, '0', MoveLeft)
    step (4, '1') = (4, '1', MoveLeft)
    step (5, '0') = (1, '1', MoveRight)
    step (5, '1') = (5, '1', MoveLeft)
    step (q, c)   = error $ show q ++ " " ++ show c

steps3 = map reprStep $ run '0' [] [] 1 step [0]
  where
    step (1, '0') = (2, '1', MoveRight)
    step (1, '1') = (3, '1', MoveLeft)
    step (2, '0') = (1, '1', MoveLeft)
    step (2, '1') = (2, '1', MoveRight)
    step (3, '0') = (2, '1', MoveLeft)
    step (3, '1') = (0, '1', Stay)

main = do
    mapM_ print $ take 10 steps1
    putStrLn ""
    mapM_ print steps2
    putStrLn ""
    mapM_ print $ take 20 steps3