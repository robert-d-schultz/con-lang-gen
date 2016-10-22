module Parse2
( parseDictionary
, parseConPhonemeInventory
, parseVowPhonemeInventory
, parseDiphPhonemeInventory
, parseSonHier
, parseGrammarSystem
, parseExponentSystems
) where

import Prelude hiding (Word)
import Data.List

import PhonemeType2
import WordGen2
import OtherData2
import GrammarType2
import MorphologyGen

-- Parse list of words to string
parseDictionary :: [SyllWord] -> String
parseDictionary wrds = intercalate "\n" (map parseWord wrds)

-- Parse Word to string
parseWord :: SyllWord -> String
parseWord (SyllWord sylls) = "/" ++ intercalate "." (map parseSyllable sylls) ++ "/"

-- Parse morpheme for use in exponent table
parseMorpheme :: Morpheme -> String
parseMorpheme (Morpheme phonemes) = str where
  str = concatMap parsePhoneme phonemes

-- Parse Syllable to string
parseSyllable :: Syllable -> String
parseSyllable (Syllable onset (Consonant a b c d) coda) = concatMap parsePhoneme onset ++ parsePhoneme (Consonant a b c d) ++ "\809" ++ concatMap parsePhoneme coda
parseSyllable (Syllable onset nucleus coda) = concatMap parsePhoneme onset ++ parsePhoneme nucleus ++ concatMap parsePhoneme coda

-- Parse Phone to string
parsePhoneme :: Phoneme -> String
parsePhoneme Blank = []
parsePhoneme (Consonant _ _ _ ipa) = ipa
parsePhoneme (Vowel _ _ _ _ ipa) = ipa
parsePhoneme (Diphthong _ _ _ _ _ _ _ _ ipa) = ipa

-- Parse the consonant inventory into html table
parseConPhonemeInventory :: [Place] -> [Manner] -> [Phonation] -> [Phoneme] -> String
parseConPhonemeInventory places manners phonations cons = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
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
    filt = filter (\(Consonant p m h s) -> p == place && m == manner && h == phonation)  cons
    output
      | not.null $ filt = csymbol $ head filt
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
parseVowPhonemeInventory :: [Height] -> [Backness] -> [Roundedness] -> [Length] -> [Phoneme]-> String
parseVowPhonemeInventory heights backs rounds lengths vows = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
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
    filt = filter (\(Vowel h b r l s) -> h == height && b == back && r == roundness && l == len) vows
    output
      | not.null $ filt = vsymbol $ head filt
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

-- Parse the diphthong inventory, should be a table in the final
parseDiphPhonemeInventory :: [Phoneme] -> String
parseDiphPhonemeInventory diphs = "<br>\nDiphthongs: /" ++ intercalate "/, /" (init fList) ++ "/, and /" ++ last fList ++ "/\n" where
  fList = filter (not . null) (map parsePhoneme diphs)

-- Parse the sonority hierarchy
parseSonHier :: [Phoneme] -> [[Phoneme]] -> String
parseSonHier vows cons = "\n\nSonority hierarchy: " ++ "\n" ++ cListv ++ "\n" ++ intercalate "\n" cListc ++ "\n" where
  fListv = map parsePhoneme vows
  cListv = intercalate ", " fListv
  fListc = map (map parsePhoneme) cons
  cListc = map (intercalate ", ") fListc


parseGrammarSystem :: GrammarSystem -> String
parseGrammarSystem gramSys = output where
  output
    | null (particles ++ exponents) = "\nNo grammatical categories manifest for nouns."
    | otherwise = "\nGrammatical categories manifest for nouns in the following ways:" ++ particles ++ exponents

  -- parse exponents
  gp | isParticle $ gSys gramSys = parseGenders $ gSys gramSys | otherwise = ""
  ap | isParticle $ aSys gramSys = parseAnimacies $ aSys gramSys | otherwise = ""
  cp | isParticle $ cSys gramSys = parseCases $ cSys gramSys | otherwise = ""
  np | isParticle $ nSys gramSys = parseNumbers $ nSys gramSys | otherwise = ""
  hp | isParticle $ hSys gramSys = parseHonorifics $ hSys gramSys | otherwise = ""
  dp | isParticle $ dSys gramSys = parseDefinitenesses $ dSys gramSys | otherwise = ""
  sp | isParticle $ sSys gramSys = parseSpecificities $ sSys gramSys | otherwise = ""
  foo = filter (not.null) [gp, ap, cp, np, hp, dp, sp]
  particles
    | null foo         = ""
    | otherwise        = "\n\tWith particles:\n\t\t\t" ++ intercalate "\n\t\t\t" foo

  -- parse particles
  ge | isExponent $ gSys gramSys = parseGenders $ gSys gramSys | otherwise = ""
  ae | isExponent $ aSys gramSys = parseAnimacies $ aSys gramSys | otherwise = ""
  ce | isExponent $ cSys gramSys = parseCases $ cSys gramSys | otherwise = ""
  ne | isExponent $ nSys gramSys = parseNumbers $ nSys gramSys | otherwise = ""
  he | isExponent $ hSys gramSys = parseHonorifics $ hSys gramSys | otherwise = ""
  de | isExponent $ dSys gramSys = parseDefinitenesses $ dSys gramSys | otherwise = ""
  se | isExponent $ sSys gramSys = parseSpecificities $ sSys gramSys | otherwise = ""
  bar = filter (not.null) [ge, ae, ce, ne, he, de, se]
  exponents
    | null bar        = ""
    | otherwise       = "\n\tWith exponents:\n\t\t\t" ++ intercalate "\n\t\t\t" bar

  isExponent :: Manifest a -> Bool
  isExponent (Exponent _) = True
  isExponent _ = False

  isParticle :: Manifest a -> Bool
  isParticle (Particle _) = True
  isParticle _ = False


parseExponentSystems :: Int -> [ExponentSystem] -> GrammarSystem -> String
parseExponentSystems 0 expSyss gramSys = ""
parseExponentSystems i expSyss gramSys = parseExponentSystems (i-1) (init expSyss) gramSys ++ parseExponentSystem (last expSyss) gs as cs ns hs ds ss where
  (gs, as, cs, ns, hs, ds, ss) = cleanGrammarSysE i gramSys

-- Parse an exponent system (a declension) into an html table
parseExponentSystem :: ExponentSystem -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Case] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Definiteness] -> [Manifest Specificity] -> String
parseExponentSystem expSys gs as cs ns hs ds ss = "<br>\n<table border=1>" ++ tHeader ++ nLabels ++ hLabels ++ dLabels ++ sLabels ++ megarows ++ "\n</table>\n" where
  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ show (length ns * length hs * length ds * length ss + 3) ++ "\">Suffix</th>\n\t</tr>"

  nLabels = "\n\t<tr>\n\t\t<th colspan=\"3\"></th>\n\t\t<th colspan=\"" ++ show (length hs * length ds * length ss) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (length hs * length ds * length ss) ++ "\">") (map parseNumber2 ns) ++ "</th>\n\t</tr>"
  hLabels = "\n\t<tr>\n\t\t<th colspan=\"3\">" ++ concat (replicate (length ns) ("</th>\n\t\t<th colspan=\"" ++ show (length ds * length ss) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (length ds * length ss) ++ "\">") (map parseHonorific2 hs)    ++ "</th>\n\t")) ++ "</tr>"
  dLabels = "\n\t<tr>\n\t\t<th colspan=\"3\">" ++ concat (replicate (length ns * length hs) ("</th>\n\t\t<th colspan=\"" ++ show (length ss) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (length ss) ++ "\">") (map parseDefiniteness2 ds) ++ "</th>\n\t")) ++ "</tr>"
  sLabels = "\n\t<tr>\n\t\t<th colspan=\"3\">" ++ concat (replicate (length ns * length hs * length ds) ("</th>\n\t\t<th colspan=\"" ++ show 1 ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show 1 ++ "\">") (map parseSpecificity2 ss) ++ "</th>\n\t")) ++ "</tr>"

  megarows = concatMap (makeMegaRow expSys as cs ns hs ds ss) gs

  makeMegaRow :: ExponentSystem -> [Manifest Animacy] -> [Manifest Case] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Definiteness] -> [Manifest Specificity] -> Manifest Gender -> String
  makeMegaRow expSys as cs ns hs ds ss g = megarow where
    megarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ show (length as * (length cs + 1) + 1) ++ "\">" ++ parseGender2 g ++ "</th>" ++ supersuperrows ++ "\n\t</tr>"
    supersuperrows = concatMap (makeSuperSuperRow expSys cs ns hs ds ss g) as

  makeSuperSuperRow :: ExponentSystem -> [Manifest Case] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Definiteness] -> [Manifest Specificity] -> Manifest Gender -> Manifest Animacy -> String
  makeSuperSuperRow expSys cs ns hs ds ss g a = supersuperrow where
    supersuperrow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ show (length cs + 1) ++ "\">" ++ parseAnimacy2 a ++ "</th>" ++ superrows ++ "\n\t\t</tr>"
    superrows = concatMap (makeSuperRow expSys ns hs ds ss g a) cs

  makeSuperRow :: ExponentSystem -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Definiteness] -> [Manifest Specificity] -> Manifest Gender -> Manifest Animacy -> Manifest Case -> String
  makeSuperRow expSys ns hs ds ss g a c = superrow where
    superrow = "\n\t\t\t<tr>\n\t\t\t\t<th>" ++ parseCase2 c ++ "</th>" ++ rows ++ "\n\t\t\t</tr>"
    rows = concatMap (makeRow expSys hs ds ss g a c) ns

  makeRow :: ExponentSystem -> [Manifest Honorific] -> [Manifest Definiteness] -> [Manifest Specificity] -> Manifest Gender -> Manifest Animacy -> Manifest Case -> Manifest Number -> String
  makeRow expSys hs ds ss g a c n = row where
    row = concatMap (makeSuperCluster expSys ds ss g a c n) hs

  makeSuperCluster :: ExponentSystem -> [Manifest Definiteness] -> [Manifest Specificity] -> Manifest Gender -> Manifest Animacy -> Manifest Case -> Manifest Number -> Manifest Honorific -> String
  makeSuperCluster expSys ds ss g a c n h  = supercluster where
    supercluster = concatMap (makeCluster expSys ss g a c n h) ds

  makeCluster :: ExponentSystem -> [Manifest Specificity] -> Manifest Gender -> Manifest Animacy -> Manifest Case -> Manifest Number -> Manifest Honorific -> Manifest Definiteness -> String
  makeCluster expSys ss g a c n h d = cluster where
    cluster = "\n\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t<td>" (map (getExponent expSys g a c n h d) ss) ++ "</td>"

  getExponent :: ExponentSystem -> Manifest Gender -> Manifest Animacy -> Manifest Case -> Manifest Number -> Manifest Honorific -> Manifest Definiteness -> Manifest Specificity -> String
  getExponent (ExponentSystem combos) g a c n h d s = output where
    filt = filter (\(morph, (gf, af, cf, nf, hf, df, sf)) -> gf == g && af == a && cf == c && nf == n && hf == h && df == d && sf == s) combos
    output
      | not.null $ filt = "–" ++ parseMorpheme (fst $ head filt)
      | otherwise = ""


parseGenders :: Manifest ([Gender], Int) -> String
parseGenders (Exponent (gs,_)) = "Gender (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseGender gs
parseGenders (Particle (gs,_)) = "Gender (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseGender gs
parseGenders NoManifest = ""

parseAnimacies :: Manifest ([Animacy], Int) -> String
parseAnimacies (Exponent (as,_)) = "Animacy (" ++ intercalate ", " (init strs)  ++ ", and " ++ last strs ++ ")" where
  strs = map parseAnimacy as
parseAnimacies (Particle (as,_)) = "Animacy (" ++ intercalate ", " (init strs)  ++ ", and " ++ last strs ++ ")" where
  strs = map parseAnimacy as
parseAnimacies NoManifest = ""

parseCases :: Manifest ([Case], Int) -> String
parseCases (Exponent (cs,_)) = "Case (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseCase cs
parseCases (Particle (cs,_)) = "Case (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseCase cs
parseCases NoManifest = ""

parseNumbers :: Manifest ([Number], Int) -> String
parseNumbers (Exponent (ns,_)) = "Number (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseNumber ns
parseNumbers (Particle (ns,_)) = "Number (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseNumber ns
parseNumbers NoManifest = ""

parseHonorifics :: Manifest ([Honorific], Int) -> String
parseHonorifics (Exponent (hs,_)) = "Honorific (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseHonorific hs
parseHonorifics (Particle (hs,_)) = "Honorific (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseHonorific hs
parseHonorifics NoManifest = ""

parseDefinitenesses :: Manifest ([Definiteness], Int) -> String
parseDefinitenesses (Exponent (ds,_)) = "Definiteness (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseDefiniteness ds
parseDefinitenesses (Particle (ds,_)) = "Definiteness (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseDefiniteness ds
parseDefinitenesses NoManifest = ""

parseSpecificities :: Manifest ([Specificity], Int) -> String
parseSpecificities (Exponent (ss,_)) = "Specificity (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseSpecificity ss
parseSpecificities (Particle (ss,_)) = "Specificity (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseSpecificity ss
parseSpecificities NoManifest = ""

parseGender :: Gender -> String
parseGender g
  | g == M   = "Masculine"
  | g == F   = "Feminine"
  | g == COM = "Common"
  | g == N   = "Neuter"

parseAnimacy :: Animacy -> String
parseAnimacy a
  | a == AN   = "Animate"
  | a == HUM  = "Human"
  | a == NHUM = "Non-Human"
  | a == ZO   = "Animal"
  | a == INAN = "Inanimate"

parseCase :: Case -> String
parseCase c
  | c == NOM   = "Nominative"
  | c == ACC   = "Accusative"
  | c == ERG   = "Ergative"
  | c == GEN   = "Genitive"
  | c == DAT   = "Dative"
  | c == LOC   = "Locative"
  | c == PREP  = "Prepositional"
  | c == ABL   = "Ablative"
  | c == INS   = "Instrumental"
  | c == VOC   = "Vocative"

parseNumber :: Number -> String
parseNumber n
  | n == SG  = "Singular"
  | n == DU  = "Dual"
  | n == TRI = "Trial"
  | n == PA  = "Paucal"
  | n == PL  = "Plural"

parseHonorific :: Honorific -> String
parseHonorific h
  | h == FAM   = "Informal"
  | h == FORM  = "Formal"

parseDefiniteness :: Definiteness -> String
parseDefiniteness d
  | d == DEF   = "Definite"
  | d == INDF = "Indefinite"

parseSpecificity :: Specificity -> String
parseSpecificity s
  | s == SPEC  = "Specific"
  | s == NSPEC = "Nonspecific"


parseGender2 :: Manifest Gender -> String
parseGender2 NoManifest = ""
parseGender2 (Exponent g) = parseGender2 (Particle g)
parseGender2 (Particle g)
  | g == M   = "Masculine"
  | g == F   = "Feminine"
  | g == COM = "Common"
  | g == N   = "Neuter"

parseAnimacy2 :: Manifest Animacy -> String
parseAnimacy2 NoManifest = ""
parseAnimacy2 (Exponent a) = parseAnimacy2 (Particle a)
parseAnimacy2 (Particle a)
  | a == AN   = "Animate"
  | a == HUM  = "Human"
  | a == NHUM = "Non-Human"
  | a == ZO   = "Animal"
  | a == INAN = "Inanimate"

parseCase2 :: Manifest Case -> String
parseCase2 NoManifest = ""
parseCase2 (Exponent c) = parseCase2 (Particle c)
parseCase2 (Particle c)
  | c == NOM   = "Nominative"
  | c == ACC   = "Accusative"
  | c == ERG   = "Ergative"
  | c == GEN   = "Genitive"
  | c == DAT   = "Dative"
  | c == LOC   = "Locative"
  | c == PREP  = "Prepositional"
  | c == ABL   = "Ablative"
  | c == INS   = "Instrumental"
  | c == VOC   = "Vocative"

parseNumber2 :: Manifest Number -> String
parseNumber2 NoManifest = ""
parseNumber2 (Exponent n) = parseNumber2 (Particle n)
parseNumber2 (Particle n)
  | n == SG  = "Singular"
  | n == DU  = "Dual"
  | n == TRI = "Trial"
  | n == PA  = "Paucal"
  | n == PL  = "Plural"

parseHonorific2 :: Manifest Honorific -> String
parseHonorific2 NoManifest = ""
parseHonorific2 (Exponent h) = parseHonorific2 (Particle h)
parseHonorific2 (Particle h)
  | h == FAM   = "Informal"
  | h == FORM  = "Formal"

parseDefiniteness2 :: Manifest Definiteness -> String
parseDefiniteness2 NoManifest = ""
parseDefiniteness2 (Exponent d) = parseDefiniteness2 (Particle d)
parseDefiniteness2 (Particle d)
  | d == DEF   = "Definite"
  | d == INDF = "Indefinite"

parseSpecificity2 :: Manifest Specificity -> String
parseSpecificity2 NoManifest = ""
parseSpecificity2 (Exponent s) = parseSpecificity2 (Particle s)
parseSpecificity2 (Particle s)
  | s == SPEC  = "Specific"
  | s == NSPEC = "Nonspecific"
