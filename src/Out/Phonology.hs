module Out.Phonology
( parseConPhonemeInventory
, parseVowPhonemeInventory
, parseDiphPhonemeInventory
) where

import Prelude hiding (Word)
import Data.List

import Data.Phoneme
import Out.Lexicon
import Out.IPA

-- Parse the consonant inventory into html table
parseConPhonemeInventory :: [Phoneme] -> String
parseConPhonemeInventory cons = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
  places = sort $ nub $ map cplace cons
  manners = sort $ nub $ map cmanner cons
  phonations = sort $ nub $ map cvoice cons
  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ show (length places * length phonations + 1) ++ "\">Consonant Inventory</th>\n\t</tr>"
  pLabels = "\n\t<tr>\n\t\t<th></th>\n\t\t<th colspan=\"" ++ show (length phonations) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (length phonations) ++ "\">") (map parsePlace places) ++ "</th>\n\t</tr>"
  clusters = concatMap (makeRow cons places phonations) manners

  makeRow :: [Phoneme] -> [Place] -> [Phonation] -> Manner -> String
  makeRow cons places phonations manner = row where
    row = "\n\t<tr>\n\t\t<th>" ++ parseManner manner ++ "</th>" ++ cluster ++ "\n\t</tr>"
    cluster = concatMap (makeCluster cons manner phonations) places

  makeCluster :: [Phoneme] -> Manner -> [Phonation] -> Place -> String
  makeCluster cons manner phonations place = cluster where
    cluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" (map (getIPASymbol cons manner place) phonations) ++ "</td>"

  getIPASymbol :: [Phoneme] -> Manner -> Place -> Phonation -> String
  getIPASymbol cons manner place phonation = output where
    filt = filter (\(Consonant p m h) -> p == place && m == manner && h == phonation) cons
    output
      | not.null $ filt = parsePhonemeIPA $ head filt
      | otherwise = ""

parseManner :: Manner -> String
parseManner m
  | m == NASAL        = "Nasal"
  | m == STOP         = "Stop"
  | m == SAFFRICATE   = "Silibant affricate"
  | m == AFFRICATE    = "Affricate"
  | m == SILIBANT     = "Silibant fricative"
  | m == FRICATIVE    = "Fricative"
  | m == APPROXIMANT  = "Approximant"
  | m == FLAP         = "Flap"
  | m == TRILL        = "Trill"
  | m == LAFFRICATE   = "Lateral affricate"
  | m == LFRICATIVE   = "Lateral fricative"
  | m == LAPPROXIMANT = "Lateral approximant"
  | m == LFLAP        = "Lateral flap"

parsePlace :: Place -> String
parsePlace p
  | p == LABIAL         = "Labial"
  | p == BILABIAL       = "Bilabial"
  | p == LABIODENTAL    = "Labiodental"
  | p == CORONAL        = "Coronal"
  | p == DENTIALVEOLAR  = "Denti-alveolar"
  | p == DENTAL         = "Dental"
  | p == ALVEOLAR       = "Alveolar"
  | p == POSTALVEOLAR   = "Post-Alveolar"
  | p == RETROFLEX      = "Retroflex"
  | p == DORSAL         = "Dorsal"
  | p == ALVEOLOPALATAL = "Alveolo-palatal"
  | p == PALATAL        = "Palatal"
  | p == VELAR          = "Velar"
  | p == UVULAR         = "Uvular"
  | p == LARYNGEAL      = "Laryngeal"
  | p == EPIPHARYNGEAL  = "Epipharyngeal"
  | p == PHARYNGEAL     = "Pharyngeal"
  | p == EPIGLOTTAL     = "Epiglottal"
  | p == GLOTTAL        = "Glottal"

parsePhonation :: Phonation -> String
parsePhonation h
  | h == VOICELESS = "Voiceless"
  | h == BREATHY   = "Breathy"
  | h == SLACK     = "Slack"
  | h == MODAL     = "Modal"
  | h == STIFF     = "Stiff"
  | h == CREAKY    = "Creaky"
  | h == ASPIRATED = "Aspirated"


-- Parse the vowel inventory into html table
parseVowPhonemeInventory :: [Phoneme] -> String
parseVowPhonemeInventory vows = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
  heights = sort $ nub $ map vheight vows
  backs = sort $ nub $ map vbackness vows
  rounds = sort $ nub $ map vroundedness vows
  lengths = sort $ nub $ map vlength vows

  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ show (length backs * length rounds + 1) ++ "\">Vowel Inventory</th>\n\t</tr>"
  pLabels = "\n\t<tr>\n\t\t<th></th>\n\t\t<th colspan=\"" ++ show (length rounds) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (length rounds) ++ "\">") (map parseBackness backs) ++ "</th>\n\t</tr>"
  clusters = concatMap (makeSuperRow vows backs rounds lengths) heights

  makeSuperRow :: [Phoneme] -> [Backness] -> [Roundedness] -> [Length] -> Height -> String
  makeSuperRow vows backs rounds lengths height = superrow where
    superrow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ show (length lengths + 1) ++ "\">" ++ parseHeight height ++ "</th>" ++ rows ++ "\n\t</tr>"
    rows = concatMap (makeRow vows rounds backs height) lengths

  makeRow :: [Phoneme] -> [Roundedness] -> [Backness] -> Height -> Length -> String
  makeRow vows rounds backs height len = cluster where
    cluster = "\n\t\t<tr>" ++ concatMap (makeCluster vows len height rounds) backs ++ "\n\t\t</tr>"

  makeCluster :: [Phoneme] ->  Length -> Height -> [Roundedness] -> Backness -> String
  makeCluster vows len height rounds back  = cluster where
    cluster = "\n\t\t\t<td>" ++ intercalate "</td>\n\t\t\t<td>" (map (getIPASymbol vows len height back) rounds) ++ "</td>"

  getIPASymbol :: [Phoneme] -> Length -> Height -> Backness -> Roundedness -> String
  getIPASymbol vows len height back roundness = output where
    filt = filter (\(Vowel h b r l _) -> h == height && b == back && r == roundness && l == len) vows
    output
      | length filt > 1 = init $ parsePhonemeIPA $ head filt
      | length filt == 1 = parsePhonemeIPA $ head filt
      | otherwise = ""

parseHeight :: Height -> String
parseHeight h
  | h == CLOSE      = "Close"
  | h == NEARCLOSE  = "Near-close"
  | h == CLOSEMID   = "Close-mid"
  | h == MID        = "Mid"
  | h == OPENMID    = "Open-mid"
  | h == NEAROPEN   = "Near-open"
  | h == OPEN       = "Open"

parseBackness :: Backness -> String
parseBackness b
  | b == BACK      = "Back"
  | b == NEARBACK  = "Near-back"
  | b == CENTRAL   = "Central"
  | b == NEARFRONT = "Near-front"
  | b == FRONT     = "Front"

parseRoundedness :: Roundedness -> String
parseRoundedness r
  | r == DEFAULT   = ""
  | r == ROUNDED   = "Rounded"
  | r == UNROUNDED = "Unrounded"

parseLength :: Length -> String
parseLength l
  | l == SHORT  = "Short"
  | l == NORMAL = ""
  | l == LONG   = "Long"

parseTone :: Tone -> String
parseTone t
  | t == NONET   = ""
  | t == TOPT    = "Top"
  | t == HIGHT   = "High"
  | t == MIDT    = "Mid"
  | t == LOWT    = "Low"
  | t == BOTTOMT = "Bottom"
  | t == FALLT   = "Falling"
  | t == HFALLT  = "High falling"
  | t == LFALLT  = "Low falling"
  | t == RISET   = "Rising"
  | t == HRISET  = "High rising"
  | t == LRISET  = "Low rising"
  | t == DIPT    = "Dipping"
  | t == PEAKT   = "Peaking"

-- Parse the diphthong inventory, should be a table in the final
parseDiphPhonemeInventory :: [Phoneme] -> String
parseDiphPhonemeInventory diphs = "<br>\nDiphthongs: /" ++ intercalate "/, /" (init fList) ++ "/, and /" ++ last fList ++ "/\n" where
  fList = filter (not . null) (map parsePhonemeIPA diphs)
