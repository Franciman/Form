{-# LANGUAGE Arrows #-}
module ValidityCheck where

import Data.Char
import FRP.Yampa
import Text.Regex.TDFA

data Form = Form { cognome :: String
                 , nome :: String
                 , dataNascita :: String
                 , comuneNascita :: String
                 , codiceFiscale :: String
                 , sesso :: String
                 , comuneResidenza :: String
                 , cap :: String
                 , indirizzo :: String
                 , telefono :: String
                 , allergie :: String
                 , gradoHandicap :: String
                 , descrizioneHandicap :: String
                 , note :: String
                 }

mainSF :: SF (Form, Event ())  (IO ())
mainSF = proc (form, addPressed) -> do
  validForm <- checkForm -< form
  addData -< (form, addPressed `tag` validForm)

addData :: SF (Form, Event Bool) (IO ())
addData = arr $ \(form, ev) -> event doNothing (addToDb form) ev
  where doNothing = return ()
        addToDb form = return ()

checkForm :: SF Form Bool
checkForm = parB
  [ checkCognome             <<^ cognome
  , checkNome                <<^ nome
  , checkDataNascita         <<^ dataNascita
  , checkComuneNascita       <<^ comuneNascita
  , checkCodiceFiscale       <<^ codiceFiscale
  , checkSesso               <<^ sesso
  , checkComuneResidenza     <<^ comuneResidenza
  , checkCap                 <<^ cap
  , checkIndirizzo           <<^ indirizzo
  , checkTelefono            <<^ telefono
  , checkGradoEDescrizione   <<^ gradoHandicap &&& descrizioneHandicap
  ] >>^ and

checkCognome :: SF String Bool
checkCognome = arr $ all (\c -> isAlpha c || isSpace c)

checkNome :: SF String Bool
checkNome = arr $ all (\c -> isAlpha c || isSpace c)

checkDataNascita :: SF String Bool
checkDataNascita = arr $ \str -> str =~ "^\\d$"

checkComuneNascita :: SF String Bool
checkComuneNascita = arr $ not . null

checkCodiceFiscale :: SF String Bool
checkCodiceFiscale = arr $ \str -> lenght str == 16 && all (\c -> isAlpha c || isDigit c) str

checkSesso :: SF String Bool
checkSesso = arr $ not . null

checkComuneResidenza :: SF String Bool
checkComuneResidenza = arr $ all (\c -> isAlpha c || isSpace c)

checkCap :: SF String Bool
checkCap :: arr $ not . null

checkIndirizzo :: SF String Bool
checkIndirizzo :: arr $ all (\c -> isAlpha c || isDigit c || isSpace c)

checkTelefono :: SF String Bool
checkTelefono = arr $ all isDigit

checkGradoHandicap :: SF String (Bool, Bool)
checkGradoHandicap = arr $ \str -> if null str
                                   then (True, False)
                                   else let b = all isDigit str in (b, b)

checkDescrizioneHandicap :: SF (String, Bool) Bool
checkDescrizioneHandicap = arr $ \(str, needed) -> if needed
                                                   then not . null $ str
                                                   else True

checkGradoEDescrizione :: SF (String, String) Bool
checkGradoEDescrizione = proc (gradoStr, descrStr) -> do
  (validGrado, descrNeeded) <- checkGradoHandicap -< gradoStr
  validDescr <- checkDescrizioneHandicap -< (descrStr, descrNeeded)
  returnA $ validGrado && validDescr
