module Database where

import qualified Database.HDBC as D
import qualified Database.HDBC.Sqlite3 as S
import Form

getComuniAndCap :: IO ([String], [String])
getComuniAndCap = do
  conn <- S.connectSqlite3 "stuff/test.db"
  D.quickQuery' conn "SELECT * from comuni" [] >>= toPair
    where toPair :: [[D.SqlValue]] -> IO ([String], [String])
	  toPair arr = return $ foldr (\xs (comuni, cap) -> ( (D.fromSql (xs !! 0) : comuni), (D.fromSql (xs !! 1) : cap) )) ([], []) arr

writeFormToDb :: Form -> IO ()
writeFormToDb form = do
  conn <- S.connectSqlite3 "stuff/test2.db"
  res <- D.prepare conn "INSERT INTO alunni values (?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
  D.execute res [ D.toSql $ cognome form
                , D.toSql $ nome form
                , D.toSql $ dataNascita form
                , D.toSql $ comuneNascita form
                , D.toSql $ codiceFiscale form
                , D.toSql $ sesso form
                , D.toSql $ comuneResidenza form
                , D.toSql $ cap form
                , D.toSql $ indirizzo form
                , D.toSql $ telefono form
                , D.toSql $ optional $ allergie form
                , D.toSql $ optional $ gradoHandicap form
                , D.toSql $ optional $ descrizioneHandicap form
                , D.toSql $ optional $ note form
                ]
  D.commit conn
  D.disconnect conn
  return ()

optional :: String -> Maybe String
optional ""  = Nothing
optional str = Just str
