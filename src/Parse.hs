{-# LANGUAGE Rank2Types #-}
module Parse
( parseDictionary
, parseWord
, parseConPhonemeInventory
, parseVowPhonemeInventory
, parseDiphPhonemeInventory
, parseSonHier
, parseLCInflection
, parseLexicalSystems
) where

import Prelude hiding (Word)
import Data.List
import GHC.Exts hiding (Word)
import Control.Arrow

import PhonemeData
import OtherData
import InflectionData
import MorphologyGen
import Syllabification
import Romanization
import GrammarData

-- Parse list of words to string
parseDictionary :: [[Phoneme]] -> [((String, LexCat), Word)] -> String
parseDictionary sonHier pairs = "\n" ++ intercalate "\n" (map (parseDictionaryEntry sonHier) (reduceHomophones pairs))

reduceHomophones :: [((String, LexCat), Word)] -> [([(String, LexCat)], Word)]
reduceHomophones pairs = map (second head . unzip) (groupWith snd (sortWith snd pairs))

parseDictionaryEntry :: [[Phoneme]] -> ([(String, LexCat)], Word) -> String
parseDictionaryEntry sonHier (means, wrd) = romanizeWord wrd ++ " (" ++ parseWord sonHier wrd ++ ")" ++ concatMap (\(str, lc) -> "\n\t" ++ parseLC lc ++ " " ++ str) means

parseLC :: LexCat -> String
parseLC lc
  | lc == Verb = "v."
  | lc == Noun = "n."
  | lc == Adj  = "adj."
  | lc == Adv  = "adv."
  | lc == Adpo = "p."

-- Parse Word to string
parseWord :: [[Phoneme]] -> Word -> String
parseWord sonHier word = "/" ++ intercalate "." (map parseSyllable sylls) ++ "/" where
  (SyllWord sylls) = syllabifyWord sonHier word

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
parsePhoneme Blank = ""
parsePhoneme (Consonant _ _ _ ipa) = ipa
parsePhoneme (Vowel _ _ _ _ _ ipa) = ipa
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
parseVowPhonemeInventory :: [Height] -> [Backness] -> [Roundedness] -> [Length] -> [Phoneme] -> String
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
    filt = filter (\(Vowel h b r l _ s) -> h == height && b == back && r == roundness && l == len) vows
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
  fList = filter (not . null) (map parsePhoneme diphs)

-- Parse the sonority hierarchy
parseSonHier :: [Phoneme] -> [[Phoneme]] -> String
parseSonHier vows cons = "\n\nSonority hierarchy: " ++ "\n/" ++ cListv ++ "/\n/" ++ intercalate "/\n/" cListc ++ "/\n" where
  fListv = map parsePhoneme vows
  cListv = intercalate "/, /" fListv
  fListc = map (map parsePhoneme) cons
  cListc = map (intercalate "/, /") fListc


-- Parse inflection system (per lex cat)
parseLCInflection :: InflectionSystem -> String
parseLCInflection inflSys = concatMap (parseInflectionSystem inflSys) [Noun, Adj, Adv, Adpo, Verb]

-- Parse inflection system (gives summary)
parseInflectionSystem :: InflectionSystem -> LexCat -> String
parseInflectionSystem inflSys lc = output where
  output
    | null (particles ++ prefixes ++ suffixes) = "<br>\nNo grammatical categories manifest for " ++ parseLexCat lc ++ "s.\n"
    | otherwise = "<br>\nGrammatical categories manifest for " ++ parseLexCat lc ++ "s in the following ways:\n<ul>" ++ particles ++ prefixes ++ suffixes ++ "</ul>\n"

-- parse particles
  filt1 = filter (not.null) (fooBar (isParticle lc) inflSys)
  particles
    | null filt1       = ""
    | otherwise        = "\n\t<li>With particles:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt1 ++ "</li>\n\t\t</ul>"

  -- parse prefixes
  filt2 = filter (not.null) (fooBar (isPrefix lc) inflSys)
  prefixes
    | null filt2      = ""
    | otherwise       = "\n\t<li>With prefixes:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt2 ++ "</li>\n\t\t</ul>"

  -- parse suffixes
  filt3 = filter (not.null) (fooBar (isSuffix lc) inflSys)
  suffixes
    | null filt3      = ""
    | otherwise       = "\n\t<li>With suffixes:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt3 ++ "</li>\n\t\t</ul>"

  fooBar :: (forall a . Manifest a -> Bool) -> InflectionSystem -> [String]
  fooBar b inflSys = [gen, ani, cas, num, def, spe, top, per, hon, pol, ten, asp, moo, voi, evi, tra, vol] where
    gen | b (genSys inflSys) = parseGenders $ genSys inflSys         | otherwise = ""
    ani | b (aniSys inflSys) = parseAnimacies $ aniSys inflSys       | otherwise = ""
    cas | b (casSys inflSys) = parseCases $ casSys inflSys           | otherwise = ""
    num | b (numSys inflSys) = parseNumbers $ numSys inflSys         | otherwise = ""
    def | b (defSys inflSys) = parseDefinitenesses $ defSys inflSys  | otherwise = ""
    spe | b (speSys inflSys) = parseSpecificities $ speSys inflSys   | otherwise = ""
    top | b (topSys inflSys) = parseTopics $ topSys inflSys          | otherwise = ""
    per | b (perSys inflSys) = parsePersons $ perSys inflSys         | otherwise = ""
    hon | b (honSys inflSys) = parseHonorifics $ honSys inflSys      | otherwise = ""
    pol | b (polSys inflSys) = parsePolarities $ polSys inflSys      | otherwise = ""
    ten | b (tenSys inflSys) = parseTenses $ tenSys inflSys          | otherwise = ""
    asp | b (aspSys inflSys) = parseAspects $ aspSys inflSys         | otherwise = ""
    moo | b (mooSys inflSys) = parseMoods $ mooSys inflSys           | otherwise = ""
    voi | b (voiSys inflSys) = parseVoices $ voiSys inflSys          | otherwise = ""
    evi | b (eviSys inflSys) = parseEvidentialities $ eviSys inflSys | otherwise = ""
    tra | b (traSys inflSys) = parseTransitivities $ traSys inflSys  | otherwise = ""
    vol | b (volSys inflSys) = parseVolitions $ volSys inflSys       | otherwise = ""

  isPrefix :: LexCat -> Manifest a -> Bool
  isPrefix _ NoManifest = False
  isPrefix lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == Prefix) t

  isSuffix :: LexCat -> Manifest a -> Bool
  isSuffix _ NoManifest = False
  isSuffix lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == Suffix) t

  isParticle :: LexCat -> Manifest a -> Bool
  isParticle _ NoManifest = False
  isParticle lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == Particle) t

parseLexicalSystems :: InflectionSystem -> (LexCat, [ManifestSystem], [ManifestSystem], [ManifestSystem]) -> String
parseLexicalSystems inflSys (lc, parts, prefs, suffs) = "<br>\n" ++ parseLexCat lc
                                                         ++ parseManifestSystems parts (length parts) inflSys
                                                         ++ parseManifestSystems prefs (length prefs) inflSys
                                                         ++ parseManifestSystems suffs (length suffs) inflSys

parseManifestSystems :: [ManifestSystem] -> Int -> InflectionSystem -> String
parseManifestSystems expSyss 0 gramSys = ""
parseManifestSystems expSyss i gramSys = parseManifestSystems (init expSyss) (i-1) gramSys ++ parseManifestSystem (last expSyss) gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol where
  ManifestSystem lc mt xs = last expSyss
  (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc mt i


-- Parse a manifestation system (particles/declensions) into an html table
-- Horizontal: Case, Gender, Animacy, Number, Honorific, Transitivity, Evidentiality, Voice, Volition (9)
-- Vertical:   Tense, Aspect, Mood, Person, Clusivity, Definiteness, Specificity, Polarity, Topic     (9)
parseManifestSystem :: ManifestSystem -> [Express Gender] -> [Express Animacy] -> [Express Case] -> [Express Number] -> [Express Definiteness] -> [Express Specificity] -> [Express Topic] -> [Express Person] -> [Express Honorific] -> [Express Polarity] -> [Express Tense] -> [Express Aspect] -> [Express Mood] -> [Express Voice] -> [Express Evidentiality] -> [Express Transitivity] -> [Express Volition] -> String
parseManifestSystem manSys gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols  = "<br>\n<table border=1>" ++ title ++ header ++ exarows ++ "\n</table>\n" where

  -- title
  hls = [length cass,length gens,length anis,length nums,length hons,length tras,length evis,length vois,length vols]
  title = "\n\t<tr>\n\t\t<th colspan=\"" ++ show (product hls + 9) ++ "\">" ++ parseSystemType manSys ++ "</th>\n\t</tr>"


  parseSystemType :: ManifestSystem -> String
  parseSystemType (ManifestSystem _ Particle _) = "Particle"
  parseSystemType (ManifestSystem _ Prefix _)   = "Prefix"
  parseSystemType (ManifestSystem _ Suffix _)   = "Suffix"

  -- header
  horLabels = [map parseCase2 cass, map parseGender2 gens, map parseAnimacy2 anis, map parseNumber2 nums, map parseHonorific2 hons, map parseTransitivity2 tras, map parseEvidentiality2 evis, map parseVoice2 vois, map parseVolition2 vols]
  header = makeHeader horLabels hls

  makeHeader :: [[String]] -> [Int] -> String
  makeHeader [] hls = ""
  makeHeader lss hls = makeLabel (length lss) (head lss) hls ++ makeHeader (tail lss) hls

  makeLabel :: Int -> [String] -> [Int] -> String
  makeLabel i ls hls = "\n\t<tr>\n\t\t<th colspan=\"9\">"
                 ++ concat (replicate (product $ take (9-i) hls) ("</th>\n\t\t<th colspan=\"" ++ show (product $ drop (10-i) hls) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (product $ drop (10-i) hls) ++ "\">") ls))
                 ++ "</th>\n\t"
                 ++ "</tr>"

  -- mega rows
  vls = [length tens,length asps,length moos,length pers,1,length defs,length spes,length pols,length tops]

  exarows = concatMap (makeExaRow manSys vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols) tens

  rowSpanN :: [Int] -> Int
  rowSpanN [] = 1
  rowSpanN ns =  head ns * rowSpanN (tail ns) + 1

  makeExaRow :: ManifestSystem -> [Int] -> [Express Aspect] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> String
  makeExaRow manSys vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten = exarow where
    exarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ show (rowSpanN (drop 1 vls)) ++ "\">" ++ parseTense2 ten ++ "</th>" ++ petarows ++ "\n\t</tr>"
    petarows = concatMap (makePetaRow manSys vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten) asps

  makePetaRow :: ManifestSystem -> [Int] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> String
  makePetaRow manSys vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp = petarow where
    petarow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 2 vls)) ++ "\">" ++ parseAspect2 asp ++ "</th>" ++ terarows ++ "\n\t\t</tr>"
    terarows = concatMap (makeTeraRow manSys vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp) moos

  makeTeraRow :: ManifestSystem -> [Int] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> String
  makeTeraRow manSys vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo = terarow where
    terarow = "\n\t\t\t<tr>\n\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 3 vls)) ++ "\">" ++ parseMood2 moo ++ "</th>" ++ gigarows ++ "\n\t\t\t</tr>"
    gigarows = concatMap (makeGigaRow manSys vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo) pers

  makeGigaRow :: ManifestSystem -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> String
  makeGigaRow manSys vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = gigarow where
    gigarow = "\n\t\t\t\t<tr>\n\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 4 vls)) ++ "\">" ++ parsePerson2 per ++ "</th>" ++ megarows ++ "\n\t\t\t\t</tr>"
    megarows = makeMegaRow manSys vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per

  makeMegaRow :: ManifestSystem -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> String
  makeMegaRow manSys vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = megarow where
    megarow = "\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 5 vls)) ++ "\">" ++ "</th>" ++ kilorows ++ "\n\t\t\t\t\t</tr>"
    kilorows = concatMap (makeKiloRow manSys vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per) defs

  makeKiloRow :: ManifestSystem -> [Int] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> String
  makeKiloRow manSys vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def = kilorow where
    kilorow = "\n\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 6 vls)) ++ "\">" ++ parseDefiniteness2 def ++ "</th>" ++ hectorows ++ "\n\t\t\t\t\t\t</tr>"
    hectorows = concatMap (makeHectoRow manSys vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def) spes

  makeHectoRow :: ManifestSystem -> [Int] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> String
  makeHectoRow manSys vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe = hectorow where
    hectorow = "\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 7 vls)) ++ "\">" ++ parseSpecificity2 spe ++ "</th>" ++ decarows ++ "\n\t\t\t\t\t\t\t</tr>"
    decarows = concatMap (makeDecaRow manSys vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe) pols

  makeDecaRow :: ManifestSystem -> [Int] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> String
  makeDecaRow manSys vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol = decarow where
    decarow = "\n\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 8 vls)) ++ "\">" ++ parsePolarity2 pol ++ "</th>" ++ rows ++ "\n\t\t\t\t\t\t\t\t</tr>"
    rows = concatMap (makeRow manSys vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol) tops

  makeRow :: ManifestSystem -> [Int] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> String
  makeRow manSys vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol top = row where
    row = "\n\t\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 9 vls)) ++ "\">" ++ parseTopic2 top ++ "</th>" ++ decirows ++ "\n\t\t\t\t\t\t\t\t\t</tr>"
    decirows = concatMap (makeDeciRow manSys gens anis nums hons tras evis vois vols ten asp moo per def spe pol top) cass

  makeDeciRow :: ManifestSystem -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> String
  makeDeciRow manSys gens anis nums hons tras evis vois vols ten asp moo per def spe pol top cas = concatMap (makeCentiRow manSys anis nums hons tras evis vois vols ten asp moo per def spe pol top cas) gens

  makeCentiRow :: ManifestSystem -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> String
  makeCentiRow manSys anis nums hons tras evis vois vols ten asp moo per def spe pol top cas gen = concatMap (makeMilliRow manSys nums hons tras evis vois vols ten asp moo per def spe pol top cas gen) anis

  makeMilliRow :: ManifestSystem -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> String
  makeMilliRow manSys nums hons tras evis vois vols ten asp moo per def spe pol top cas gen ani = concatMap (makeMicroRow manSys hons tras evis vois vols ten asp moo per def spe pol top cas gen ani) nums

  makeMicroRow :: ManifestSystem -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> String
  makeMicroRow manSys hons tras evis vois vols ten asp moo per def spe pol top cas gen ani num = concatMap (makeNanoRow manSys tras evis vois vols ten asp moo per def spe pol top cas gen ani num) hons

  makeNanoRow :: ManifestSystem -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> String
  makeNanoRow manSys tras evis vois vols ten asp moo per def spe pol top cas gen ani num hon = concatMap (makePicoRow manSys evis vois vols ten asp moo per def spe pol top cas gen ani num hon) tras

  makePicoRow :: ManifestSystem -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> String
  makePicoRow manSys evis vois vols ten asp moo per def spe pol top cas gen ani num hon tra = concatMap (makeFemtoRow manSys vois vols ten asp moo per def spe pol top cas gen ani num hon tra) evis

  makeFemtoRow :: ManifestSystem -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> String
  makeFemtoRow manSys vois vols ten asp moo per def spe pol top cas gen ani num hon tra evi = concatMap (makeAttoRow manSys vols ten asp moo per def spe pol top cas gen ani num hon tra evi) vois

  makeAttoRow :: ManifestSystem -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> String
  makeAttoRow manSys vols ten asp moo per def spe pol top cas gen ani num hon tra evi voi = cluster where
    cluster = "\n\t\t\t\t\t\t\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t\t\t\t\t\t\t<td>" (map (getMorpheme manSys ten asp moo per def spe pol top cas gen ani num hon tra evi voi) vols) ++ "</td>"

  getMorpheme :: ManifestSystem -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Express Volition -> String
  getMorpheme (ManifestSystem _ Particle combos) ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | not.null $ filt = parseMorpheme (fst $ head filt)
      | otherwise = ""
  getMorpheme (ManifestSystem _ Prefix combos) ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | not.null $ filt = parseMorpheme (fst $ head filt) ++ "–"
      | otherwise = ""
  getMorpheme (ManifestSystem _ Suffix combos) ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | not.null $ filt = "–" ++ parseMorpheme (fst $ head filt)
      | otherwise = ""

parseGenders :: Manifest [Gender] -> String
parseGenders (Manifest _ gens) = "Gender (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseGender gens
parseGenders NoManifest = ""

parseAnimacies :: Manifest [Animacy] -> String
parseAnimacies (Manifest _ anis) = "Animacy (" ++ intercalate ", " (init strs)  ++ ", and " ++ last strs ++ ")" where
  strs = map parseAnimacy anis
parseAnimacies NoManifest = ""

parseCases :: Manifest [Case] -> String
parseCases (Manifest _ cass) = "Case (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseCase cass
parseCases NoManifest = ""

parseNumbers :: Manifest [Number] -> String
parseNumbers (Manifest _ nums) = "Number (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseNumber nums
parseNumbers NoManifest = ""

parseDefinitenesses :: Manifest [Definiteness] -> String
parseDefinitenesses (Manifest _ defs) = "Definiteness (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseDefiniteness defs
parseDefinitenesses NoManifest = ""

parseSpecificities :: Manifest [Specificity] -> String
parseSpecificities (Manifest _ spes) = "Specificity (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseSpecificity spes
parseSpecificities NoManifest = ""

parseTopics :: Manifest [Topic] -> String
parseTopics (Manifest _ tops) = "Topic (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseTopic tops
parseTopics NoManifest = ""

parsePersons :: Manifest [Person] -> String
parsePersons (Manifest _ pers) = "Person (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parsePerson pers
parsePersons NoManifest = ""

parseHonorifics :: Manifest [Honorific] -> String
parseHonorifics (Manifest _ hons) = "Honorific (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseHonorific hons
parseHonorifics NoManifest = ""

parsePolarities :: Manifest [Polarity] -> String
parsePolarities (Manifest _ pols) = "Polarity (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parsePolarity pols
parsePolarities NoManifest = ""

parseTenses :: Manifest [Tense] -> String
parseTenses (Manifest _ tens) = "Tense (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseTense tens
parseTenses NoManifest = ""

parseAspects :: Manifest [Aspect] -> String
parseAspects (Manifest _ asps) = "Aspect (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseAspect asps
parseAspects NoManifest = ""

parseMoods :: Manifest [Mood] -> String
parseMoods (Manifest _ moos) = "Mood (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseMood moos
parseMoods NoManifest = ""

parseVoices :: Manifest [Voice] -> String
parseVoices (Manifest _ vois) = "Voice (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseVoice vois
parseVoices NoManifest = ""

parseEvidentialities :: Manifest [Evidentiality] -> String
parseEvidentialities (Manifest _ evis) = "Evidentiality (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseEvidentiality evis
parseEvidentialities NoManifest = ""

parseTransitivities :: Manifest [Transitivity] -> String
parseTransitivities (Manifest _ tras) = "Transitivity (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseTransitivity tras
parseTransitivities NoManifest = ""

parseVolitions :: Manifest [Volition] -> String
parseVolitions (Manifest _ vols) = "Volition (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseVolition vols
parseVolitions NoManifest = ""

parseGender :: Gender -> String
parseGender gen
  | gen == UGEN = "Unknown"
  | gen == M   = "Masculine"
  | gen == F   = "Feminine"
  | gen == COM = "Common"
  | gen == N   = "Neuter"

parseAnimacy :: Animacy -> String
parseAnimacy ani
  | ani == UANI = "Unknown"
  | ani == AN   = "Animate"
  | ani == HUM  = "Human"
  | ani == NHUM = "Non-Human"
  | ani == ZO   = "Animal"
  | ani == INAN = "Inanimate"

parseCase :: Case -> String
parseCase cas
  | cas == UCAS = "Unknown"
  | cas == INTR = "Intransitive"
  | cas == ACC = "Accusative"
  | cas `elem` [ERG, ERG2] = "Ergative"
  | cas == PEG = "Pegative"
  | cas == INDIR = "Indirective"
  | cas == SEC = "Secundative"
  | cas `elem` [NOM, NOM2] = "Nominative"
  | cas `elem` [ABS, ABS2, ABS3] = "Absolutive"
  | cas == MTR = "Monotransitive"
  | cas == DIR = "Directive"
  | cas == PRIM = "Primative"
  | cas == DTR = "Ditransitive"
  | cas == OBJ = "Objective"
  | cas `elem` [DRT1, DRT2] = "Direct"
  | cas == TR = "Transitive"
  | cas == ADP = "Adpositional"
  | cas == PREP = "Prepositional"
  | cas == POST = "Postpositional"
  | cas `elem` [OBL1, OBL2, OBL3, OBL4, OBL5, OBL6] = "Oblique"

parseNumber :: Number -> String
parseNumber num
  | num == UNUM = "Unknown"
  | num == SG  = "Singular"
  | num == DU  = "Dual"
  | num == TRI = "Trial"
  | num == PA  = "Paucal"
  | num == PL  = "Plural"

parseDefiniteness :: Definiteness -> String
parseDefiniteness def
  | def == UDEF = "Unknown"
  | def == DEF  = "Definite"
  | def == INDF = "Indefinite"

parseSpecificity :: Specificity -> String
parseSpecificity spe
  | spe == USPE = "Unknown"
  | spe == SPEC  = "Specific"
  | spe == NSPEC = "Nonspecific"

parseTopic :: Topic -> String
parseTopic top
  | top == UTOP = "Unknown"
  | top == TOP  = "Topic"
  | top == NTOP = "Not topic"

parsePerson :: Person -> String
parsePerson per
  | per == UPER = "Unknown"
  | per == FIRST  = "First"
  | per == FSTINCL = "First inclusive"
  | per == FSTEXCL = "First exclusive"
  | per == SECOND = "Second"
  | per == THIRD  = "Third"
  | per == THRDPROX  = "Proximate"
  | per == THRDOBV  = "Obviative"

parseHonorific :: Honorific -> String
parseHonorific hon
  | hon == UHON  = "Unknown"
  | hon == FAM   = "Informal"
  | hon == NEU   = "Neutral"
  | hon == FORM  = "Formal"

parsePolarity :: Polarity -> String
parsePolarity pol
  | pol == UPOL = "Unknown"
  | pol == AFF = "Affirmative"
  | pol == NEG = "Negative"

parseTense :: Tense -> String
parseTense ten
  | ten == UTEN = "Unknown"
  | ten == PST  = "Simple past"
  | ten == PRS  = "Simple present"
  | ten == FUT  = "Simple future"
  | ten == APRS = "Anterior present"
  | ten == APST = "Anterior past"
  | ten `elem` [AFUT, AFUT1, AFUT2, AFUT3] = "Anterior future"
  | ten == PPRS = "Posterior present"
  | ten == PFUT = "Posterior future"
  | ten `elem` [PPST, PPST1, PPST2, PPST3] = "Posterior past"
  | ten == PSTPER  = "Past perfect"
  | ten == PRSPER  = "Present perfect"
  | ten == FUTPER  = "Future perfect"

parseAspect :: Aspect -> String
parseAspect asp
  | asp == UASP = "Unknown"
  | asp == NNPROG = "Not progressive"
  | asp == PFV  = "Perfective"
  | asp == IPFV = "Imperfective"
  | asp == HAB = "Habitual"
  | asp == CONT = "Continuous"
  | asp == NPROG = "Non-progressive"
  | asp == PROG = "Progressive"


parseMood :: Mood -> String
parseMood moo
  | moo == UMOO = "Unknown"
  | moo == IND  = "Indicative"
  | moo == IRR  = "Irrealis"
  | moo == DEO  = "Deontic"
  | moo == IMP  = "Imperative"
  | moo == JUS  = "Jussive"
  | moo == OPT  = "Optative"
  | moo == EPIS = "Epistemic"
  | moo == SBJV = "Subjunctive"
  | moo == POT  = "Potential"
  | moo == COND = "Conditional"

parseVoice :: Voice -> String
parseVoice voi
  | voi == UVOI = "Unknown"
  | voi == ACTIVE  = "Active"
  | voi == MIDDLE  = "Middle"
  | voi == PASSIVE = "Passive"

parseEvidentiality :: Evidentiality -> String
parseEvidentiality evi
  | evi == UEVI  = "Unknown"
  | evi == EXP   = "Witness"
  | evi == VIS   = "Visual"
  | evi == NVIS  = "Non-visual"
  | evi == AUD   = "Auditory"
  | evi == INFER = "Inferential"
  | evi == REP   = "Reportative"
  | evi == HSY   = "Hearsay"
  | evi == QUO   = "Quotative"
  | evi == ASS   = "Assumed"

parseTransitivity :: Transitivity -> String
parseTransitivity tra
  | tra == UTRA    = "Unknown"
  | tra == NTRANS  = "Intransitive"
  | tra == TRANS   = "Transitive"
  | tra == MTRANS  = "Monotransitive"
  | tra == DITRANS = "Ditransitive"

parseVolition :: Volition -> String
parseVolition vol
  | vol == UVOL = "Unknown"
  | vol == VOL  = "Intended"
  | vol == NVOL = "Unintended"

parseGender2 :: Express Gender -> String
parseGender2 NoExpress = ""
parseGender2 (Express gen) = parseGender gen

parseAnimacy2 :: Express Animacy -> String
parseAnimacy2 NoExpress = ""
parseAnimacy2 (Express ani) = parseAnimacy ani

parseCase2 :: Express Case -> String
parseCase2 NoExpress = ""
parseCase2 (Express cas) = parseCase cas

parseNumber2 :: Express Number -> String
parseNumber2 NoExpress = ""
parseNumber2 (Express num) = parseNumber num

parseDefiniteness2 :: Express Definiteness -> String
parseDefiniteness2 NoExpress = ""
parseDefiniteness2 (Express def) = parseDefiniteness def

parseSpecificity2 :: Express Specificity -> String
parseSpecificity2 NoExpress = ""
parseSpecificity2 (Express spe) = parseSpecificity spe

parseTopic2 :: Express Topic -> String
parseTopic2 NoExpress = ""
parseTopic2 (Express top) = parseTopic top

parsePerson2 :: Express Person -> String
parsePerson2 NoExpress = ""
parsePerson2 (Express per) = parsePerson per

parseHonorific2 :: Express Honorific -> String
parseHonorific2 NoExpress = ""
parseHonorific2 (Express hon) = parseHonorific hon

parsePolarity2 :: Express Polarity -> String
parsePolarity2 NoExpress = ""
parsePolarity2 (Express pol) = parsePolarity pol

parseTense2 :: Express Tense -> String
parseTense2 NoExpress = ""
parseTense2 (Express ten) = parseTense ten

parseAspect2 :: Express Aspect -> String
parseAspect2 NoExpress = ""
parseAspect2 (Express asp) = parseAspect asp

parseMood2 :: Express Mood -> String
parseMood2 NoExpress = ""
parseMood2 (Express moo) = parseMood moo

parseVoice2 :: Express Voice -> String
parseVoice2 NoExpress = ""
parseVoice2 (Express voi) = parseVoice voi

parseEvidentiality2 :: Express Evidentiality -> String
parseEvidentiality2 NoExpress = ""
parseEvidentiality2 (Express evi) = parseEvidentiality evi

parseTransitivity2 :: Express Transitivity -> String
parseTransitivity2 NoExpress = ""
parseTransitivity2 (Express tra) = parseTransitivity tra

parseVolition2 :: Express Volition -> String
parseVolition2 NoExpress = ""
parseVolition2 (Express vol) = parseVolition vol

parseLexCat :: LexCat -> String
parseLexCat lc
  | lc == Subj = "Subject"
  | lc == Obj  = "Object"
  | lc == Noun = "Noun"
  | lc == Adj  = "Adjective"
  | lc == Adv  = "Adverb"
  | lc == Adpo = "Adposition"
  | lc == Verb = "Verb"
