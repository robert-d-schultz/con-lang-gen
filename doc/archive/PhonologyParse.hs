module PhonologyGen
(parsePhonology
) where

-- Import
import Prelude

-- Data structures
import PhonemeType2

parsePhonology :: ConsonantPhonemeInventory -> String
parsePhonology inv = output where
  ConsonantPhonemeInventory consList = inv
  output = parseConsonantList consList "Consonant descriptions:\n"

parseConsonantList :: [Consonant] -> String -> String
parseConsonantList consList str = parseConsonantList x (str ++ "Consonant description:\n" ++ parseConsonant a) where
  a:x = consList

parseConsonant :: Consonant -> String
parseConsonant cons = output where

  getPlaceOutput (NoContour placeU1)       = parsePlaceMain placeU1 []
  getPlaceOutput (Contour placeU1 placeU2) = parsePlaceMain placeU1 placeU2
  placeOutput = getPlaceOutput (place cons)

  getMannerOutput (NoContour mannerU1)        = parseManner mannerU1 []
  getMannerOutput (Contour mannerU1 mannerU2) = parseManner mannerU1 mannerU2
  mannerOutput = getMannerOutput (manner cons)

  getAirstreamOutput (NoContour airstreamU1)           = parseAirstream airstreamU1 []
  getAirstreamOutput (Contour airstreamU1 airstreamU2) = parseAirstream airstreamU1 airstreamU2
  airstreamOutput = getAirstreamOutput (airstream cons)

  getPhonationOutput (NoContour phonationU1)           = parsePhonationMain phonationU1 []
  getPhonationOutput (Contour phonationU1 phonationU2) = parsePhonationMain phonationU1 phonationU2
  phonationOutput = getPhonationOutput (phonation cons)

  output = placeOutput ++ "\n" ++ mannerOutput ++ "\n"  ++ airstreamOutput ++ "\n"  ++ phonationOutput

parsePlaceMain :: Place -> Place -> String
parsePlaceMain place1 place2  = mainOutput where
  activeOutput = parseActive $ active place1
  passiveOutput = parsePassive $ passive place1
  output = parsePlace place1

  mainOutput = "* The place of articulation is " ++ output ++ "which means it is articulated with the " ++
    activeOutput ++ " and the " ++ passiveOutput ++ "."

parsePlaceMain place1 place2 = mainOutput where
  activeOutput1 = parseActive $ active place1
  passiveOutput1 = parsePassive $ passive place1

  activeOutput2 = parseActive $ active place2
  passiveOutput2 = parsePassive $ passive place2

  output1 = parsePlace place1
  output2 = parsePlace place2

  mainOutput = "* The consonant is coarticulated. The first place of articulation is " ++
    output1 ++ ", which means it is articulated with the " ++ activeOutput1 ++ " and the " ++
    passiveOutput1 ++ ". The second place of articulation is " ++ output2 ++
    ", which means it is articulated with the " ++ activeOutput2 ++ " and the " ++ passiveOutput2 ++ "."

parsePlace :: Place -> String
parsePlace (Place LOWERLIP UPPERLIP)       = "bilabial"
parsePlace (Place LOWERLIP UPPERTEETH)     = "labiodental"
parsePlace (Place TONGUEBLADE UPPERLIP)    = "linguolabial"
parsePlace (Place TONGUEBLADE UPPERTEETH)  = "interdental"
parsePlace (Place TONGUEBLADE TEETHRIDGE)  = "denti-alveolar"
parsePlace (Place TONGUEBLADE RIDGE)       = "laminal alveolar"
parsePlace (Place TONGUEBLADE BACKRIDGE)   = "palato-alveolar"
parsePlace (Place TONGUETIP UPPERTEETH)    = "dental"
parsePlace (Place TONGUETIP RIDGE)         = "apico-alveolar"
parsePlace (Place TONGUETIP BACKRIDGE)     = "apical retroflex"
parsePlace (Place TONGUEUNDER HARDPALATE)  = "retroflex"
parsePlace (Place TONGUEBODY BACKRIDGE)    = "alveolo-palatal"
parsePlace (Place TONGUEBODY HARDPALATE)   = "palatal"
parsePlace (Place TONGUEBODY SOFTPALATE)   = "velar"
parsePlace (Place TONGUEBODY UVULA)        = "uvular"
parsePlace (Place TONGUEROOT PHARYNX)      = "pharyngeal"
parsePlace (Place LARYNX PHARYNX)          = "epiglotto-pharyngeal"
parsePlace (Place LARYNX EPIGLOTTIS)       = "epiglottal"
parsePlace (Place LARYNX GLOTTIS)          = "glottal"
parsePlace _                             = "ERROR"

parseActive :: ActiveArticulator -> String
parseActive LOWERLIP    = "lower lip"
parseActive TONGUEBLADE = "blade of the tongue"
parseActive TONGUETIP   = "tip of the tongue"
parseActive TONGUEUNDER = "underside of the tongue"
parseActive TONGUEBODY  = "body of the tongue"
parseActive TONGUEROOT  = "root of the tongue"
parseActive LARYNX      = "larynx"
parseActive _           = "ERROR"

parsePassive :: PassiveArticulator -> String
parsePassive UPPERTEETH = "lower lip"
parsePassive TEETHRIDGE = "upper teeth/alveolar ridge"
parsePassive RIDGE      = "alveolar ridge"
parsePassive BACKRIDGE  = "back of the alveolar ridge"
parsePassive HARDPALATE = "hard palate"
parsePassive SOFTPALATE = "soft palate"
parsePassive UVULA      = "uvula"
parsePassive PHARYNX    = "pharynx"
parsePassive EPIGLOTTIS = "epiglottis"
parsePassive GLOTTIS    = "glottis"
parsePassive _          = ""

parseMannerMain :: Manner -> Manner -> String
parseMannerMain manner1 [] = output where
  strictureOutput1 = parseStricture $ stricture manner1

  trillOutput1 = parseTrill $ trill manner1

  lengthOutput1 = parseLength $ mannerLength manner1

  lateralityOutput1 = parseLaterality $ laterality manner1

  silibanceOutput1 = parseSilibance $ silibance manner1

  airescapeOutput1 = parseAirescape $ airescape manner1

  votOutput1 = parseVot $ vot manner1

  output = strictureOutput1 ++ trillOutput1 ++ lengthOutput1 ++ lateralityOutput1 ++
    silibanceOutput1 ++ airescapeOutput1 ++ votOutput1

--needs work
parseMannerMain manner1 manner2 = output where
  output = "."
--more

parseManner :: Manner -> String
parseManner Manner OCCLUSION NOTTRILLED SHORT LATERAL _ ORAL _ = "lateral flap"
parseManner Manner OCCLUSION NOTTRILLED SHORT _ _ NASALIZED _  = "nasal flap"
parseManner Manner OCCLUSION NOTTRILLED _ _ _ NASALIZED _      = "nasal stop"
parseManner Manner OCCLUSION NOTTRILLED SHORT _ _ _ _          = "flap"
parseManner Manner OCCLUSION NOTTRILLED _ _ _ _ _              = "stop"
parseManner Manner TURBULENT _ _ _ SILIBANT _ _                = "silibant fricative"
parseManner Manner TURBULENT _ _ LATERAL NONSILIBANT _ _       = "lateral fricative"
parseManner Manner TURBULENT _ _ _ NONSILIBANT _ _             = "non-silibant fricative"
parseManner Manner TURBULENT TRILLED _ _ _ _ _                 = "trill"
parseManner Manner SLIGHTTURBULENT _ _ LATERAL _ _ _           = "lateral approximate"
parseManner Manner SLIGHTTURBULENT _ _ _ _ _ _                 = "approximate"
parseManner Manner _ _ _ _ _ _ _                               = "ERROR?"

parseStricture :: Stricture -> String
parseStricture OCCLUSION      = "occluded"
parseStricture TURBULENT      = ""
parseStricture SLIGHTTURBULENT = ""
parseStricture _              = "ERROR"

parseTrill :: Trill -> String
parseTrill TRILLED    = "trilled"
parseTrill NOTTRILLED = ""
parseTrill _          = ""

parseLength :: Length -> String
parseLength SHORT  = "short"
parseLength NORMAL = "normal"
parseLength LONG   = "long"
parseLength _      = ""

parseLaterality :: Laterality -> String
parseLaterality LATERAL    = "lateral"
parseLaterality NONLATERAL = "central"
parseLaterality _          = ""

parseSilibance :: Silibance -> String
parseSilibance SILIBANT    = "silibant"
parseSilibance NONSILIBANT = "non-silibant"
parseSilibance _           = ""

parseAirescape :: AirEscape -> String
parseAirescape NASALIZED = "nasal"
parseAirescape ORAL      = "oral"
parseAirescape           = ""

parseVot :: VOT -> String
parseVot POSITIVE = "positive"
parseVot ZERO     = "zero"
parseVot NEGATIVE = "negative"
parseVot _        = ""

parseAirstreamMain :: Airstream -> Airstream -> String
parseAirstreamMain airstream1 [] = output where
  initiatorOutput1 = parseInitiator1 $ initiator airstream1
  initiatorOutput2 = parseInitiator2 $ initiator airstream1
  directionOutput1 = parseDirection1 $ direction airstream1
  directionOutput2 = parseDirection2 $ direction airstream1

  fooBar Airstream PULMONIC EGRESSIVE = barFoo "pulmonic consonant"
  fooBar Airstream LINGUAL INGRESSIVE = barFoo "click consonant"
  fooBar Airstream GLOTTIC INGRESSIVE = barFoo "implosive consonant"
  fooBar Airstream GLOTTIC EGRESSIVE  = barFoo "ejective consonant"
  foobar _                            = barFoo []

  barFoo [] = "* Its air stream mechanic is " ++  initiatorOutput1 ++ " " ++
    directionOutput1 ++ " which means it is articulated by " ++ directionOutput2 ++
    " using the " ++ initiatorOutput2 ++ "."
  barFoo str = "* It is a " ++ str ++ ", meaning its air stream mechanic is " ++  initiatorOutput1 ++ " " ++
    directionOutput1 ++ " which means it is articulated by " ++ directionOutput2 ++
    " using the " ++ initiatorOutput2 ++ "."

  output = fooBar airstream1

parseAirstreamMain airstream1 airstream2 = output where
  output = "."

parseAirstream :: Airstream -> String
parseAirstream airstream1 = output where

  output = "."

--more
parseInitiator1 :: Initiator -> String
parseInitiator1 LINGUAL  = "lingual"
parseInitiator1 GLOTTIC  = "glottic"
parseInitiator1 PULMONIC = "pulmonic"
parseInitiator1 _        = "ERROR"

parseInitiator2 :: Initiator -> String
parseInitiator2 LINGUAL  = "tongue"
parseInitiator2 GLOTTIC  = "glottis"
parseInitiator2 PULMONIC = "lungs and diaphragm"
parseInitiator2 _        = "ERROR"

parseDirection1 :: Direction -> String
parseDirection1 INGRESSIVE = "ingressive"
parseDirection1 EGRESSIVE  = "egressive"
parseDirection1 _          = "ERROR"

parseDirection2 :: Direction -> String
parseDirection2 INGRESSIVE = "pulling air in"
parseDirection2 EGRESSIVE  = "pushing air out"
parseDirection2 _          = ""

parsePhonationMain :: Phonation -> Phonation -> String
parsePhonationMain phonation1 [] = output where
  output = "* Its phonation is " ++ parsePhonation1 phonation1 ++ ", meaning the vocal cords are " ++
    parsePhonation2 phonation1 ++ "."

parsePhonationMain phonation1 phonation2 = output where
  output = "* Its phonation forms a contour changing from " ++ parsePhonation1 phonation1 ++
    " to " ++ parsePhonation1 phonation2 ++ ". The vocal cords change from being " ++
    parsePhonation2 phonation2 ++ " to " ++ parsePhonation2 phonation2 ++ "."

parsePhonation1 :: Phonation -> String
parsePhonation1 VOICELESS = "voiceless"
parsePhonation1 BREATHY   = "breathy voice"
parsePhonation1 SLACK     = "slack voice"
parsePhonation1 MODAL     = "modal voice"
parsePhonation1 STIFF     = "stiff voice"
parsePhonation1 CREAKY    = "creaky voice"
parsePhonation1 CLOSURE   = "glottal closure"
parsePhonation1 _         = "ERROR"

parsePhonation2 :: Phonation -> String
parsePhonation2 VOICELESS = "completely relaxed"
parsePhonation2 BREATHY   = "slightly narrower than in voiceless"
parsePhonation2 SLACK     = "slightly wider than in modal voice"
parsePhonation2 MODAL     = "vibrated at maximum"
parsePhonation2 STIFF     = "slightly narrower than in modal voice"
parsePhonation2 CREAKY    = "slightly wider than in glottal closure"
parsePhonation2 CLOSURE   = "pressed together"
parsePhonation2 _         = "ERROR"
