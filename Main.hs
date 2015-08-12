module Main where

import Graphics.UI.Gtk
import FRP.Yampa
import ValidityCheck


checkValidity :: SF (Event EventData) [String] 
checkValidity = formValidator

main :: IO ()
main = do
  initGUI
  builder <- builderNew
  builderAddFromFile builder "stuff/Win.glade"
  window <- builderGetObject builder castToWindow "window1"
  window `on` objectDestroy $ mainQuit
  reactHandle <- reactInit (return NoEvent) (\ _ _ b -> ((mapM_ putStrLn b) >> putStrLn "-----") >> return False) checkValidity
  initHandlers reactHandle builder
  widgetShowAll window
  mainGUI


initHandlers :: ReactHandle (Event EventData) b -> Builder -> IO ()
initHandlers handle builder = mapM_ initHandler fields
  where fields = ["cognome", "nome", "data", "codiceFiscale", "comuneResidenza", "indirizzo", "telefono", "grado", "descrizione"]
	initHandler :: String -> IO ()
	initHandler entryName = do
	  entry <- builderGetObject builder castToEntry (entryName ++ "Entry")
	  entry `on` editableChanged $ makeReact entry entryName
	  return ()
	makeReact :: Entry -> String -> IO ()
	makeReact entry entryName = do
	  content <- entryGetText entry
	  let eventData = EventData { text = content, eventSource = entryName }
          react handle (0, Just $ Event eventData)
	  return ()
