data Move = Stay | MoveLeft | MoveRight

run                             -- run a Turing Machine
  :: Eq q
  => c                          -- the blank symbol
  -> [c]                        -- the input tape
  -> ((q, c) -> (q, c, Move))   -- the transition function
  -> q                          -- the starting state
  -> [q]                        -- the acceptance states
  -> [(q, [c], [c])]            -- the list of all future states
run b s d q0 f = run' (repeat b) (s ++ repeat b) d q0 f

run'                            -- run a Turing Machine
  :: Eq q
  => [c]                        -- the tape to the left
  -> [c]                        -- the tape to the right including head
  -> ((q, c) -> (q, c, Move))   -- the transition function
  -> q                          -- the starting state
  -> [q]                        -- the acceptance states
  -> [(q, [c], [c])]            -- the list of all future states
run' as bs d q0 f =
  case runOnce as bs d q0 f of
    Nothing             -> []   -- halted
    Just (q', as', bs') -> (q', as', bs') : run' as' bs' d q' f

runOnce                         -- run a single step of a Turing Machine
  :: Eq q
  => [c]                        -- the tape to the left
  -> [c]                        -- the tape to the right including head
  -> ((q, c) -> (q, c, Move))   -- the transition function
  -> q                          -- the starting state
  -> [q]                        -- the acceptance states
  -> Maybe (q, [c], [c])        -- the next state, or Nothing if halted
runOnce (a:as) (b:bs) d q0 f
    | q0 `elem` f = Nothing     -- halted
    | otherwise   =
      case d (q0, b) of
        (q', b', Stay)      -> Just (q', a:as, b':bs)
        (q', b', MoveLeft)  -> Just (q', as, a:b':bs)
        (q', b', MoveRight) -> Just (q', b':a:as, bs)