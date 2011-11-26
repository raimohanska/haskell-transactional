module Transactional(Transactional, Connection, transactionally, getConnection) where

data Connection = Connection
data Transactional a = Transactional (Connection -> IO a)

instance Monad Transactional where
  (>>=) op toNext = Transactional $ \conn -> perform conn op >>= perform conn . toNext
  return a = Transactional $ return . const a

getConnection :: Transactional Connection
getConnection = Transactional $ return

perform :: Connection -> Transactional a -> IO a
perform conn (Transactional op) = op conn

newConnection :: IO Connection
newConnection = undefined

commit :: Connection -> IO ()
commit = undefined

transactionally :: Transactional a -> IO a
transactionally (Transactional op) = do
  conn <- newConnection
  result <- op conn
  commit conn 
  return result
