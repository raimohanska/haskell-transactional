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
