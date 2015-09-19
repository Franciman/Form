{-# LANGUAGE Arrows #-}
module ValidityCheck where

import Data.Char
import FRP.Yampa
import Text.Regex.TDFA
import Graphics.UI.Gtk
import Form
import Database

mainSF :: SF (Form, Event ()) ExecAction
mainSF = proc (form, addPressed) -> do
  (checkExecAction, validForm) <- checkForm -< form
  addExecAction <- addData -< (form, addPressed `tag` validForm)
  returnA -< checkExecAction *>* addExecAction

addData :: SF (Form, Event Bool) ExecAction
addData = arr $ \(form, ev) -> event doNothing (addToDb form) ev
  where doNothing :: ExecAction
        doNothing = \builder -> return ()
        addToDb :: Form -> Bool -> ExecAction
        addToDb form b = \builder -> if b
                                     then writeFormToDb form >> showSuccess builder
                                     else showError builder

showSuccess :: ExecAction
showSuccess builder = do
  mainWin <- builderGetObject builder castToWindow "window1"
  msgDialog <- messageDialogNew (Just mainWin) [DialogModal] MessageInfo ButtonsClose "Alunno aggiunto con successo"
  dialogRun msgDialog
  widgetDestroy msgDialog
  return ()

showError :: ExecAction
showError builder = do
  mainWin <- builderGetObject builder castToWindow "window1"
  msgDialog <- messageDialogNew (Just mainWin) [DialogModal] MessageError ButtonsClose "Ci sono degli errori, l'alunno non è stato aggiunto"
  dialogRun msgDialog
  widgetDestroy msgDialog
  return ()

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
  ] >>^ foldr (\(action, val) (actAccum, valAccum) -> (action *>* actAccum, val && valAccum)) (\builder -> return (), True)

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

checkCompulsoryField :: String -> (String -> Bool) -> String -> SF String (ExecAction, Bool)
checkCompulsoryField errorLabelName pred errorMsg = arr $ \str -> if null str
                                                                  then setError errorLabelName "Questo campo è obbligatorio"
                                                                  else if pred str
                                                                       then unsetError errorLabelName
                                                                       else setError errorLabelName errorMsg

checkCognome :: SF String (ExecAction, Bool)
checkCognome = checkCompulsoryField "cognomeError" pred "Cognome non valido"
  where pred = all (\c -> isAlpha c || isSpace c)

checkNome :: SF String (ExecAction, Bool)
checkNome = checkCompulsoryField "nomeError" pred "Nome non valido"
  where pred = all (\c -> isAlpha c || isSpace c)

checkDataNascita :: SF String (ExecAction, Bool)
checkDataNascita = checkCompulsoryField "dataError" pred "La data deve essere nel formato gg/mm/aaaa"
  where pred :: String -> Bool
        pred str = str =~ "^([[:digit:]]{2})/([[:digit:]]{2})/([[:digit:]]{4})$"

checkComuneNascita :: SF String (ExecAction, Bool)
checkComuneNascita = checkCompulsoryField "comuneNascitaError" (const True) ""

checkCodiceFiscale :: SF String (ExecAction, Bool)
checkCodiceFiscale = checkCompulsoryField "codiceFiscaleError" pred "Codice fiscale non valido"
  where pred str = length str == 16 && all (\c -> isAlpha c || isDigit c) str

checkSesso :: SF String (ExecAction, Bool)
checkSesso = checkCompulsoryField "sessoError" (const True) ""

checkComuneResidenza :: SF String (ExecAction, Bool)
checkComuneResidenza = checkCompulsoryField "comuneResidenzaError" pred "Comune non valido"
  where pred str = all (\c -> isAlpha c || isSpace c) str

checkCap :: SF String (ExecAction, Bool)
checkCap = checkCompulsoryField "capError" (const True) ""

checkIndirizzo :: SF String (ExecAction, Bool)
checkIndirizzo = checkCompulsoryField "indirizzoError" pred "Indirizzo non valido"
  where pred str = all (\c -> isAlpha c || isDigit c || isSpace c) str

checkTelefono :: SF String (ExecAction, Bool)
checkTelefono = checkCompulsoryField "telefonoError"  (all isDigit) "Numero di telefono non valido"

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
