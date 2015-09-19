{-# LANGUAGE Arrows #-}
module ValidityCheck where

import Data.Char
import FRP.Yampa
import Text.Regex.TDFA
import Graphics.UI.Gtk

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
  (checkExecAction, validForm) <- checkForm -< form
  addExecAction <- addData -< (form, addPressed `tag` validForm)
  returnA -< checkExecAction *>* addExecAction

addData :: SF (Form, Event Bool) (IO ())
addData = arr $ \(form, ev) -> event doNothing (addToDb form) ev
  where doNothing :: IO ()
        doNothing = return ()
        addToDb :: Form -> Bool -> IO ()
        addToDb form b = putStrLn $ show b

type ExecAction = Builder -> IO ()
(*>*) :: ExecAction -> ExecAction -> ExecAction
a1 *>* a2 = \builder -> a1 builder >> a2 builder

checkForm :: SF Form (ExecAction, Bool)
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
  ] >>^ foldr (\(action, val) (actAccum, valAccum) -> (action >> actAccum, val && valAccum)) (return ())

setError :: String -> String -> (ExecAction, Bool)
setError errorLabelName errorMsg = (action, False)
  where action :: ExecAction
        action builder = do
          label <- builderGetObject builder castToLabel errorLabelName
          labelSetText label errorMsg
          return ()

unsetError :: String -> (ExecAction, Bool)
unsetError errorLabelName = (action , True)
  where action :: ExecAction
        action builder = do
          label <- builderGetObject builder castToLabel errorLabelName
          labelSetText label ""
          return ()

checkCognome :: SF String (ExecAction, Bool)
checkCognome = arr $ if all (\c -> isAlpha c || isSpace c)
                     then unsetError "cognomeError"
                     else setError "cognomeError" "C'è qualche carattere non valido"

checkNome :: SF String (ExecAction, Bool)
checkNome = arr $ if all (\c -> isAlpha c || isSpace c)
                  then unsetError "nomeError"
                  else setError "nomeError" "C'è qualche carattere non valido"

checkDataNascita :: SF String (ExecAction, Bool)
checkDataNascita = arr $ \str -> str =~ "^\\d$"

checkComuneNascita :: SF String (ExecAction, Bool)
checkComuneNascita = arr $ if not . null
                           then unsetError "comuneNascitaError"
                           else setError "comuneNascitaError" "Inserire un comune"

checkCodiceFiscale :: SF String (ExecAction, Bool)
checkCodiceFiscale = arr $ if \str -> length str == 16 && all (\c -> isAlpha c || isDigit c) str
                           then unsetError "codiceFiscaleError"
                           else setError "codiceFiscaleError" "C'è qualche carattere non valido"

checkSesso :: SF String (ExecAction, Bool)
checkSesso = arr $ if not . null
                   then unsetError "sessoError"
                   else setError "sessoError" "Inserire sesso"

checkComuneResidenza :: SF String (ExecAction, Bool)
checkComuneResidenza = arr $ if all (\c -> isAlpha c || isSpace c)
                             then unsetError "comuneResidenzaError"
                             else setError "comuneResidenzaError" "C'è qualche carattere non valido"

checkCap :: SF String (ExecAction, Bool)
checkCap = arr $ if not . null
                   then unsetError "capError"
                   else setError "capError" "Inserire cap"

checkIndirizzo :: SF String (ExecAction, Bool)
checkIndirizzo = arr $ if all (\c -> isAlpha c || isDigit c || isSpace c)
                       then unsetError "indirizzoError"
                       else setError "indirizzoError" "C'è qualche carattere non valido"

checkTelefono :: SF String (ExecAction, Bool)
checkTelefono = arr $ if all isDigit
                      then unsetError "telefonoError"
                      else setError "telefonoError" "Numero di telefono non valido"

checkGradoHandicap :: SF String ((ExecAction, Bool), Bool)
checkGradoHandicap = arr $ \str -> if null str
                                   then (unsetError "gradoError", False)
                                   else if all isDigit str
                                        then (unsetError "gradoError", True)
                                        else (setError "gradoError" "Grado non valido", False)

checkDescrizioneHandicap :: SF (String, Bool) (ExecAction, Bool)
checkDescrizioneHandicap = arr $ \(str, needed) -> if needed
                                                   then if not . null $ str
                                                        then unsetError "descrizioneError"
                                                        else setError "descrizioneError" "Descrizione necessaria"
                                                   else unsetError "descrizioneError"

checkGradoEDescrizione :: SF (String, String) (ExecAction, Bool)
checkGradoEDescrizione = proc (gradoStr, descrStr) -> do
  ((gradoExecAction, validGrado), descrNeeded) <- checkGradoHandicap -< gradoStr
  (descrExecAction, validDescr) <- checkDescrizioneHandicap -< (descrStr, descrNeeded)
  returnA -< (gradoExecAction *>* descrExecAction ,validGrado && validDescr)
