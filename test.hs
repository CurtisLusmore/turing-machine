import Turing

reprStep (q, as, bs) = (reverse $ take 5 as) ++ take 5 bs

steps = map reprStep $ run 0 [] step 'b' []
  where
    step ('b', _) = ('c', 1, MoveRight)
    step ('c', _) = ('e', 0, MoveRight)
    step ('e', _) = ('f', 2, MoveRight)
    step ('f', _) = ('b', 0, MoveRight)

main = do
    mapM_ (putStrLn . show) $ take 10 steps