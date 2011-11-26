module Transactional(Transaction, Commitable, DataSource(), transactionally, getConnection) where

import Data.IORef
import Control.Exception

data Commitable = Commitable {commit :: IO (), rollback :: IO ()}
type State = IORef [Commitable]
data Transaction a = Transaction (State -> IO a)

data DataSource a = DataSource { newConnection :: IO (a, Commitable) } 

toCommitable :: IO () -> IO () -> Commitable
toCommitable commit rollback = Commitable commit rollback

instance Monad Transaction where
  (>>=) op toNext = Transaction $ \state -> perform state op >>= \a -> perform state (toNext a)
  return a = Transaction $ \st -> return a

perform :: State -> Transaction a -> IO a
perform state (Transaction op) = op state

getConnection :: DataSource a -> Transaction a
getConnection ds = Transaction $ \state -> do (newConn, commitable) <- newConnection ds
                                              modifyIORef state (commitable :)
                                              return newConn

transactionally :: Transaction a -> IO a
transactionally (Transaction op) = bracketOnError (newIORef [])
                                                  rollbackAll
                                                  performTransaction
  where performTransaction state = do result <- op state 
                                      commitables <- readIORef state
                                      mapM_ commit commitables 
                                      return result
        rollbackAll state = readIORef state >>= mapM_ rollback
