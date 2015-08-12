{-# LANGUAGE Arrows #-}
module ValidityCheck where

import FRP.Yampa
import Data.Char
import qualified Text.Regex.TDFA as R

data EventData = EventData {
          text :: String,
          eventSource :: String
     } deriving (Eq, Show)

validCF :: String -> Bool
validCF str = all isAlphaNum str

validName :: String -> Bool
validName str = all (\c -> isAlpha c || isSpace c) str

validNumber :: String -> Bool
validNumber str = R.matchTest regex str
  where regex = R.makeRegex "^([[:digit:]]){10}$" :: R.Regex

validDate :: String -> Bool
validDate str = R.matchTest regex str
  where regex = R.makeRegex "^([[:digit:]]+)/([[:digit:]]+)/([[:digit:]]+)$" :: R.Regex

validGrade :: String -> (Event (), Bool)
validGrade str =  let res = R.matchTest regex str
		  in (if res then Event () else NoEvent, str == "" || res)
  where regex = R.makeRegex "^(100|0?[0-9]{1,2})$" :: R.Regex

isValid :: (String -> a) -> a -> (Event String) -> a
isValid validator oldValue = event oldValue validator

nomeValidator :: SF (Event String) Bool
nomeValidator = sscan (isValid validName) False

cognomeValidator :: SF (Event String) Bool
cognomeValidator = sscan (isValid validName) False

dataValidator :: SF (Event String) Bool
dataValidator = sscan (isValid validDate) False

cfValidator :: SF (Event String) Bool
cfValidator = sscan (isValid validCF) False

comuneValidator :: SF (Event String) Bool
comuneValidator = sscan (isValid validName) False

indirizzoValidator :: SF (Event String) Bool
indirizzoValidator = sscan (isValid validName) False

telefonoValidator :: SF (Event String) Bool
telefonoValidator = sscan (isValid validNumber) False

gradoValidator :: SF (Event String) (Event (), Bool)
gradoValidator = sscan (isValid validGrade) (NoEvent, True)

descrizioneValidator :: SF (Event (), Event String) Bool
descrizioneValidator  = sscanPrim validate False True
  where validate :: Bool -> (Event (), Event String) -> Maybe (Bool, Bool)
	validate notEmpty (validGrado, ev) = event unchanged check ev
	  where unchanged :: Maybe (Bool, Bool)
	        unchanged = Just (notEmpty, event True (\() -> notEmpty) validGrado)
	        check :: String -> Maybe (Bool, Bool)
	        check str = Just (not . null $ str, correct)
		  where correct :: Bool
	                correct = event True (\() -> not . null $ str) validGrado


changed :: String -> SF (Event EventData) (Event String)
changed sourceName = arr $ event NoEvent getContent
  where getContent :: EventData -> Event String
	getContent evData = if eventSource evData == sourceName
	                    then Event $ text evData
	                    else NoEvent

gradoEdescrizione :: SF (Event EventData) Bool
gradoEdescrizione = proc ev  -> do
  (required, b1) <- gradoValidator <<< changed "grado" -< ev
  b2 <- descrizioneValidator <<< (second $ changed "descrizione") -< (required, ev)
  returnA -< and [b1, b2]

formValidator :: SF (Event EventData) [String]
formValidator = parB checkers >>^ printa
  where checkers = [changed "cognome" >>> cognomeValidator,
	            changed "nome" >>> nomeValidator,
		    changed "data" >>> dataValidator,
		    changed "codiceFiscale" >>> cfValidator,
		    changed "comuneResidenza" >>> comuneValidator,
		    changed "indirizzo" >>> indirizzoValidator,
		    changed "telefono" >>> telefonoValidator,
		    gradoEdescrizione]
        printa xs = zipWith printer ["cognome ", "nome ", "data ", "codiceFiscale ", "comuneResidenza ", "indirizzo ", "telefono ", "gradoEdescr "] xs
	  where printer n b = n ++ show b
