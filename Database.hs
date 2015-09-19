module Database where

import qualified Database.HDBC as D
import qualified Database.HDBC.Sqlite3 as S

getComuniAndCap :: IO ([String], [String])
getComuniAndCap = do
  conn <- S.connectSqlite3 "stuff/test.db"
  D.quickQuery' conn "SELECT * from comuni" [] >>= toPair 
    where toPair :: [[D.SqlValue]] -> IO ([String], [String])
	  toPair arr = return $ foldr (\xs (comuni, cap) -> ( (D.fromSql (xs !! 0) : comuni), (D.fromSql (xs !! 1) : cap) )) ([], []) arr

--writeFormToDb :: Form -> IO ()
--writeFormToDb form = return ()
