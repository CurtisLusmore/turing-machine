import Turing

reprStep n (q, as, b:bs) = (reverse $ take n as) ++ "[" ++ (b:"") ++ "]" ++ take (n-1) bs
reprStep' = reprStep 5

startswith []     _      = True
startswith _      []     = False
startswith (a:as) (b:bs) = a == b && startswith as bs

steps1 = map reprStep' $ run ' ' [] [] 1 step (const False)
  where
    step (1, _) = (2, '0', MoveRight)
    step (2, _) = (3, ' ', MoveRight)
    step (3, _) = (4, '1', MoveRight)
    step (4, _) = (1, ' ', MoveRight)

steps2 = map reprStep' $ run '0' [] ['1', '1'] 1 step (==0)
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

steps3 = map reprStep' $ run '0' [] [] 1 step (==0)
  where
    step (1, '0') = (2, '1', MoveRight)
    step (1, '1') = (3, '1', MoveLeft)
    step (2, '0') = (1, '1', MoveLeft)
    step (2, '1') = (2, '1', MoveRight)
    step (3, '0') = (2, '1', MoveLeft)
    step (3, '1') = (0, '1', Stay)

steps4 = map (reprStep 20) $ run ' ' "" "11 111 1111 1" "find" step (startswith "halt")
  where
    step ("find", 'a') = ("find", '1', MoveRight)
    step ("find", 'b') = ("find", '1', MoveRight)
    step ("find", '1') = ("find", '1', MoveRight)
    step ("find", ' ') = ("find-check-1", ' ', MoveRight)
    step ("find", b)   = ("find", b, MoveRight)

    step ("find-check-1", '1') = ("insert", '1', MoveLeft)
    step ("find-check-1", ' ') = ("clear-done-l", ' ', MoveLeft)
    step ("find-check-1", b)   = ("halt-error", b, Stay)

    step ("clear-done-l", b) = ("clear-done", b, MoveLeft)

    step ("clear-done", '.') = ("clear-done", ' ', MoveLeft)
    step ("clear-done", 'a') = ("clear-done", '1', MoveLeft)
    step ("clear-done", 'b') = ("clear-done", '1', MoveLeft)
    step ("clear-done", ' ') = ("halt-sorted", ' ', MoveRight)
    step ("clear-done", b)   = ("clear-done", b, MoveLeft)

    step ("insert", ' ') = ("insert-mB", '.', MoveLeft)
    step ("insert", '.') = ("insert-mB", '.', MoveLeft)

    step ("insert-mB", ' ') = ("sort", ' ', MoveRight)
    step ("insert-mB", '.') = ("sort", '.', MoveRight)
    step ("insert-mB", b)   = ("insert-mB", b, MoveLeft)

    step ("sort", b) = ("match-a", b, Stay)

    step ("match-a", '.') = ("clean-then-find", '.', Stay)
    step ("match-a", '1') = ("match-1-.", 'a', MoveRight)
    step ("match-a", b)   = ("halt-error", b, Stay)

    step ("match-1-.", '.') = ("match-1", '.', MoveRight)
    step ("match-1-.", b)   = ("match-1-.", b, MoveRight)

    step ("clean-then-find", '.') = ("clean-then-find", '.', MoveLeft)
    step ("clean-then-find", 'b') = ("clean-then-find", '1', MoveLeft)
    step ("clean-then-find", b)   = ("find", b, MoveRight)

    step ("match-1", 'a') = ("match-1", 'a', MoveRight)
    step ("match-1", '1') = ("matchL-a-.", 'a', MoveLeft)
    step ("match-1", b)   = ("need-swap", b, MoveLeft)

    step ("matchL-a-.", '.') = ("matchL-a", '.', MoveLeft)
    step ("matchL-a-.", b)   = ("matchL-a-.", b, MoveLeft)

    step ("matchL-a", 'a') = ("match-a", 'b', MoveRight)
    step ("matchL-a", b)   = ("matchL-a", b, MoveLeft)

    step ("need-swap", b) = ("need-swap-.", b, Stay)

    step ("need-swap-.", 'a') = ("need-swap-.", '1', MoveLeft)
    step ("need-swap-.", '1') = ("need-swap-.", '1', MoveLeft)
    step ("need-swap-.", '.') = ("need-swap-.", '1', MoveLeft)
    step ("need-swap-.", 'b') = ("need-swap-p.", 'b', MoveRight)

    step ("need-swap-p.", _) = ("need-swap-b", '.', MoveLeft)

    step ("need-swap-b", 'b') = ("need-swap-b", '1', MoveLeft)
    step ("need-swap-b", '.') = ("insert", '.', Stay)
    step ("need-swap-b", ' ') = ("find", ' ', MoveRight)
    step ("need-swap-b", b)   = ("halt-error", b, MoveLeft)

main = do
    mapM_ putStrLn $ take 10 steps1
    putStrLn ""
    mapM_ putStrLn steps2
    putStrLn ""
    mapM_ putStrLn $ take 20 steps3
    putStrLn ""
    mapM_ putStrLn steps4