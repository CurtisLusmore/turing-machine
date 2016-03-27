module Turing where

data Move = Stay | MoveLeft | MoveRight

run                             -- run a Turing Machine
  :: c                          -- the blank symbol
  -> [c]                        -- the input tape to the left
  -> [c]                        -- the input tape to the right including head
  -> q                          -- the starting state
  -> ((q, c) -> (q, c, Move))   -- the transition function
  -> (q -> Bool)                -- the acceptance function
  -> [(q, [c], [c])]            -- the list of all future states
run b as bs q0 d f = (q0, as', bs') : run' as' bs' q0 d f
  where
    as' = as ++ repeat b
    bs' = bs ++ repeat b
    run' as bs q0 d f =
      case runOnce as bs q0 d f of
        Nothing             -> []   -- halted
        Just (q', as', bs') -> (q', as', bs') : run' as' bs' q' d f
    runOnce (a:as) (b:bs) q0 d f
        | f q0      = Nothing       -- halted
        | otherwise =
          case d (q0, b) of
            (q', b', Stay)      -> Just (q', a:as, b':bs)
            (q', b', MoveLeft)  -> Just (q', as, a:b':bs)
            (q', b', MoveRight) -> Just (q', b':a:as, bs)