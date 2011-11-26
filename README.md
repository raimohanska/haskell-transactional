Transaction Monad
=================

Monad for managed transactions. Defines the type `DataSource` for getting arbitrary "connections" and `Commitable` for
connections that can be either commited or rolled back. Automatically commits all connections after successful execution.
Performs rollback on all connections if any exception is thrown.

The `Example.hs` file gives an example dummy datasource implementation and demonstrates Transaction monad usage. 
It's as simple as this:

~~~ .haskell
module Example where

import Transactional

data Connection = Connection { query :: String -> Transaction String }

dummyDataSource :: DataSource (Connection)
dummyDataSource = DataSource $ return (Connection dummyQuery, dummyCommitable)

dummyQuery "DELETE FROM USERS" = do
  liftIO $ putStrLn $ "querying.."
  return "lol"

dummyCommitable = toCommitable (putStrLn "commit") (putStrLn "rollback")

okTransaction :: IO ()
okTransaction = transactionally $ do
  conn <- getConnection dummyDataSource
  queryResult <- query conn "DELETE FROM USERS"
  liftIO $ putStrLn $ "got result: " ++ queryResult 

failingTransaction :: IO ()
failingTransaction = transactionally $ do
  conn <- getConnection dummyDataSource
  query conn "DROP TABLE USERS"
  return ()
~~~

If you run this in GHCI, you see how connections are commited and rolled back automatically:

~~~ .haskell
*Example> okTransaction 
querying..
got result: lol
commit

*Example> failingTransaction 
rollback
*** Exception: Example.hs:(10,1)-(12,14): Non-exhaustive patterns in function dummyConnection
~~~