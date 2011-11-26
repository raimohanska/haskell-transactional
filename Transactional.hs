module Transactional(Transaction, Commitable, DataSource(), transactionally) where

data Commitable = Commitable {commit :: IO (), rollback :: IO ()}
type State = [Commitable]
data Transaction a = Transaction (State -> IO (a, State))

data DataSource a = DataSource { getConnection :: IO (a, Commitable) } 

toCommitable :: IO () -> IO () -> Commitable
toCommitable commit rollback = Commitable commit rollback

instance Monad Transaction where
  (>>=) op toNext = Transaction $ \state -> perform state op >>= \(a, state2) -> perform state2 (toNext a)
  return a = Transaction $ \st -> return (a, st)

perform :: State -> Transaction a -> IO (a, State)
perform state (Transaction op) = op state

transactionally :: Transaction a -> IO a
transactionally (Transaction op) = do
  (result, commitables) <- op []
  mapM_ commit commitables 
  return result
