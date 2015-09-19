module Main where

import Graphics.UI.Gtk
import FRP.Yampa
import ValidityCheck
import Database
import qualified Data.Text as T (pack)
import Control.Monad (zipWithM_)
import Data.Time.Clock

populate :: ComboBox -> [String] -> IO ()
populate combo f = do
  comboBoxSetModelText combo
  mapM_ (comboBoxAppendText combo . T.pack) f

entryFields :: [String]
entryFields = ["cognome", "nome", "data", "codiceFiscale", "comuneResidenza", "indirizzo", "telefono", "grado", "descrizione"]

comboFields :: [String]
comboFields = ["comuneNascita", "sesso", "cap"]

main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "stuff/Win.glade"
  window <- builderGetObject builder castToWindow "window1"
  window `on` objectDestroy $ mainQuit
  (comuni, caps) <- getComuniAndCap
  comuneNascitaCombo <- builderGetObject builder castToComboBox "comuneNascitaCombo"
  populate comuneNascitaCombo comuni
  capCombo <- builderGetObject builder castToComboBox "capCombo"
  populate capCombo caps
  populate comuneNascitaCombo capCombo
  reactHandle <- reactInit getForm  (\ _ _ b -> b >> return False) mainSF
  initHandlers reactHandle builder
  widgetShowAll window
  mainGUI

initHandlers :: ReactHandle (Form, Event ())  (IO ()) -> Builder -> IO ()
initHandlers reactHandle builder = do
    timeref <- getCurrentTime >>= newIORef
    mapM_ (handleEntry timeref) entryFields
    mapM_ (handleCombo timeRef) comboFields
    handleAddButton timeref

  where handleEntry :: IORef UTCTime -> String -> IO ()
        handleEntry timeref entryName = do
          entry <- builderGetObject builder castToEntry (entryName ++ "Entry")
          entry `on` editableChanged $ makeReact NoEvent timeref reactHandle builder
          return ()

        handleCombo :: IORef UTCTime -> String -> IO ()
        handleCombo timeref comboName = do
          combo <- builderGetObject castToComboBox (comboName ++ "Combo")
          combo `on` changed $ makeReact NoEvent timeref reactHandle builder
          return ()

       handleAddButton :: IORef UTCTime -> IO ()
       handleAddButton timeref = do
         button <- builderGetObject castToButton "aggiungiButton"
         button `on` buttonActivated $ makeReact (Event ()) timeref reactHandle builder
         return ()

makeReact :: Event () -> IORef UTCTime -> ReactHandle (Form, Event ())  (IO ()) -> Builder -> IO ()
makeReact buttonPressed timeref handle builder = do
  oldTime <- readIORef timeref
  currTime <- getCurrentTime
  writeIORef currTime
  let dt = diffUTCTime oldTime currTime
  form <- getForm builder
  react handle (dt, Just (form, buttonPressed))


getForm :: Builder -> IO Form
getForm builder = Form <$> entryContent "cognomeEntry"
                       <*> entryContent "nomeEntry"
                       <*> entryContent "dataEntry"
                       <*> comboBoxEntry "comuneNascitaCombo"
                       <*> entryContent "codiceFiscaleEntry"
                       <*> comboBoxEntry "sessoCombo"
                       <*> entryContent "comuneResidenzaEntry"
                       <*> comboBoxEntry "capCombo"
                       <*> entryContent "indirizzoEntry"
                       <*> entryContent "telefonoEntry"
                       <*> entryContent "allergieEntry"
                       <*> entryContent "gradoEntry"
                       <*> entryContent "descrizioneEntry"
                       <*> entryContent "noteEntry"
  where entryContent :: String -> IO String
        entryContent entryName = builderGetObject builder castToEntry entryName >>= entryGetText
        comboBoxEntry :: String -> IO String
        comboBoxEntry comboName = builderGetObject builder castToComboBox comboName >>= comboBoxGetActiveText >>= maybe "" id
