{-# LANGUAGE Rank2Types #-}
module Parse
( parseDictionary
, parseConPhonemeInventory
, parseVowPhonemeInventory
, parseDiphPhonemeInventory
, parseSonHier
, parseLCInflection
, parseLexicalSystems
) where

import Prelude hiding (Word)
import Data.List

import PhonemeData
import OtherData
import InflectionData
import MorphologyGen
import Syllabification
import Romanization

-- Parse list of words to string
parseDictionary :: [[Phoneme]] -> [Word] -> String
parseDictionary sonHier wrds = "\n" ++ intercalate "\n" (map (parseWord sonHier) wrds)

-- Parse Word to string
parseWord :: [[Phoneme]] -> Word -> String
parseWord sonHier word = "/" ++ intercalate "." (map parseSyllable sylls) ++ "/" ++ " - " ++ romanizeWord word where
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
parseSonHier vows cons = "\n\nSonority hierarchy: " ++ "\n/" ++ cListv ++ "/\n/" ++ intercalate "/\n/" cListc ++ "/\n" where
  fListv = map parsePhoneme vows
  cListv = intercalate "/, /" fListv
  fListc = map (map parsePhoneme) cons
  cListc = map (intercalate "/, /") fListc

parseLCInflection :: InflectionSystem -> String
parseLCInflection inflSys = concatMap (parseInflectionSystem inflSys) [Sub, Obj, Adj, Adv, Prep, Verb]

-- Parse inflection system (gives summary)
parseInflectionSystem :: InflectionSystem -> LexicalCategory -> String
parseInflectionSystem inflSys lc = output where
  output
    | null (preparticles ++ postparticles ++ prefixes ++ suffixes) = "<br>\nNo grammatical categories manifest for " ++ parseLexicalCategory lc ++ "s.\n"
    | otherwise = "<br>\nGrammatical categories manifest for " ++ parseLexicalCategory lc ++ "s in the following ways:\n<ul>" ++ preparticles ++ postparticles ++ prefixes ++ suffixes ++ "</ul>\n"

-- parse pre-position particles
  filt1 = filter (not.null) (fooBar (isPreParticle lc) inflSys)
  preparticles
    | null filt1       = ""
    | otherwise        = "\n\t<li>With pre-position particles:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt1 ++ "</li>\n\t\t</ul>"

  -- parse post-position particles
  filt2 = filter (not.null) (fooBar (isPostParticle lc) inflSys)
  postparticles
    | null filt2       = ""
    | otherwise        = "\n\t<li>With post-position particles:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt2 ++ "</li>\n\t\t</ul>"

  -- parse prefixes
  filt4 = filter (not.null) (fooBar (isPrefix lc) inflSys)
  prefixes
    | null filt4      = ""
    | otherwise       = "\n\t<li>With prefixes:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt4 ++ "</li>\n\t\t</ul>"

  -- parse suffixes
  filt3 = filter (not.null) (fooBar (isSuffix lc) inflSys)
  suffixes
    | null filt3      = ""
    | otherwise       = "\n\t<li>With suffixes:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt3 ++ "</li>\n\t\t</ul>"

  fooBar :: (forall a . Manifest a -> Bool) -> InflectionSystem -> [String]
  fooBar b inflSys = [gen, ani, cas, num, def, spe, top, per, clu, hon, pol, ten, asp, moo, voi, evi, tra, vol] where
    gen | b (genSys inflSys) = parseGenders $ genSys inflSys | otherwise = ""
    ani | b (aniSys inflSys) = parseAnimacies $ aniSys inflSys | otherwise = ""
    cas | b (casSys inflSys) = parseCases $ casSys inflSys | otherwise = ""
    num | b (numSys inflSys) = parseNumbers $ numSys inflSys | otherwise = ""
    def | b (defSys inflSys) = parseDefinitenesses $ defSys inflSys | otherwise = ""
    spe | b (speSys inflSys) = parseSpecificities $ speSys inflSys | otherwise = ""
    top | b (topSys inflSys) = parseTopics $ topSys inflSys | otherwise = ""
    per | b (perSys inflSys) = parsePersons $ perSys inflSys | otherwise = ""
    clu | b (cluSys inflSys) = parseClusivities $ cluSys inflSys | otherwise = ""
    hon | b (honSys inflSys) = parseHonorifics $ honSys inflSys | otherwise = ""
    pol | b (polSys inflSys) = parsePolarities $ polSys inflSys | otherwise = ""
    ten | b (tenSys inflSys) = parseTenses $ tenSys inflSys | otherwise = ""
    asp | b (aspSys inflSys) = parseAspects $ aspSys inflSys | otherwise = ""
    moo | b (mooSys inflSys) = parseMoods $ mooSys inflSys | otherwise = ""
    voi | b (voiSys inflSys) = parseVoices $ voiSys inflSys | otherwise = ""
    evi | b (eviSys inflSys) = parseEvidentialities $ eviSys inflSys | otherwise = ""
    tra | b (traSys inflSys) = parseTransitivities $ traSys inflSys | otherwise = ""
    vol | b (volSys inflSys) = parseVolitions $ volSys inflSys | otherwise = ""

  isSuffix :: LexicalCategory -> Manifest a -> Bool
  isSuffix _ NoManifest = False
  isSuffix lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == Suffix) t

  isPrefix :: LexicalCategory -> Manifest a -> Bool
  isPrefix _ NoManifest = False
  isPrefix lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == Prefix) t

  isPreParticle :: LexicalCategory -> Manifest a -> Bool
  isPreParticle _ NoManifest = False
  isPreParticle lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == PreParticle) t

  isPostParticle :: LexicalCategory -> Manifest a -> Bool
  isPostParticle _ NoManifest = False
  isPostParticle lc (Manifest t _) = out where
    out
      | null filt = False
      | otherwise = True
    filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == PostParticle) t


parseLexicalSystems :: InflectionSystem -> (LexicalCategory, [ManifestSystem], [ManifestSystem], [ManifestSystem], [ManifestSystem]) -> String
parseLexicalSystems inflSys (lc, preps, posps, prefs, suffs) = "<br>\n" ++ parseLexicalCategory lc
                                                            ++ parseManifestSystems preps (length preps) inflSys
                                                            ++ parseManifestSystems posps (length posps) inflSys
                                                            ++ parseManifestSystems prefs (length prefs) inflSys
                                                            ++ parseManifestSystems suffs (length suffs) inflSys

parseManifestSystems :: [ManifestSystem] -> Int -> InflectionSystem -> String
parseManifestSystems expSyss 0 gramSys = ""
parseManifestSystems expSyss i gramSys = parseManifestSystems (init expSyss) (i-1) gramSys ++ parseManifestSystem (last expSyss) gen ani cas num def spe top per clu hon pol ten asp moo voi evi tra vol where
  ManifestSystem lc mt xs = last expSyss
  (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanGrammarSys gramSys lc mt i


-- Parse a manifestation system (particles/declensions) into an html table
-- Horizontal: Case, Gender, Animacy, Number, Honorific, Transitivity, Evidentiality, Voice, Volition (9)
-- Vertical:   Tense, Aspect, Mood, Person, Clusivity, Definiteness, Specificity, Polarity, Topic     (9)
parseManifestSystem :: ManifestSystem -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Case] -> [Manifest Number] -> [Manifest Definiteness] -> [Manifest Specificity] -> [Manifest Topic] -> [Manifest Person] -> [Manifest Clusivity] -> [Manifest Honorific] -> [Manifest Polarity] -> [Manifest Tense] -> [Manifest Aspect] -> [Manifest Mood] -> [Manifest Voice] -> [Manifest Evidentiality] -> [Manifest Transitivity] -> [Manifest Volition] -> String
parseManifestSystem manSys gens anis cass nums defs spes tops pers clus hons pols tens asps moos vois evis tras vols  = "<br>\n<table border=1>" ++ title ++ header ++ exarows ++ "\n</table>\n" where

  -- title
  hls = [length cass,length gens,length anis,length nums,length hons,length tras,length evis,length vois,length vols]
  title = "\n\t<tr>\n\t\t<th colspan=\"" ++ show (product hls + 9) ++ "\">" ++ parseSystemType manSys ++ "</th>\n\t</tr>"


  parseSystemType :: ManifestSystem -> String
  parseSystemType (ManifestSystem _ PreParticle _)  = "Pre-position Particle"
  parseSystemType (ManifestSystem _ PostParticle _) = "Post-position Particle"
  parseSystemType (ManifestSystem _ Prefix _)       = "Prefix"
  parseSystemType (ManifestSystem _ Suffix _)       = "Suffix"

  -- header
  horLabels = [map parseCase2 cass, map parseGender2 gens, map parseAnimacy2 anis, map parseNumber2 nums, map parseHonorific2 hons, map parseTransitivity2 tras, map parseEvidentiality2 evis, map parseVoice2 vois, map parseVolition2 vols]
  header = makeHeader horLabels hls

  makeHeader :: [[String]] -> [Int] -> String
  makeHeader [] hls = ""
  makeHeader lss hls = makeLabel (length lss) (head lss) hls ++ makeHeader (tail lss) hls

  makeLabel :: Int -> [String] -> [Int] -> String
  makeLabel i ls hls = "\n\t<tr>\n\t\t<th colspan=\"9\">"
                 ++ concat (replicate (product $ take (9-i) hls) ("</th>\n\t\t<th colspan=\"" ++ show (product $ drop (10-i) hls) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (product $ drop (10-i) hls) ++ "\">") ls ++ "</th>\n\t"))
                 ++ "</tr>"

  -- mega rows
  vls = [length tens,length asps,length moos,length pers,length clus,length defs,length spes,length pols,length tops]

  exarows = concatMap (makeExaRow manSys vls asps moos pers clus defs spes pols tops cass gens anis nums hons tras evis vois vols) tens

  rowSpanN :: [Int] -> Int
  rowSpanN [] = 1
  rowSpanN ns =  head ns * rowSpanN (tail ns) + 1

  makeExaRow :: ManifestSystem -> [Int] -> [Manifest Aspect] -> [Manifest Mood] -> [Manifest Person] -> [Manifest Clusivity] -> [Manifest Definiteness] -> [Manifest Specificity] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> String
  makeExaRow manSys vls asps moos pers clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten = exarow where
    exarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ show (rowSpanN (drop 1 vls)) ++ "\">" ++ parseTense2 ten ++ "</th>" ++ petarows ++ "\n\t</tr>"
    petarows = concatMap (makePetaRow manSys vls moos pers clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten) asps

  makePetaRow :: ManifestSystem -> [Int] -> [Manifest Mood] -> [Manifest Person] -> [Manifest Clusivity] -> [Manifest Definiteness] -> [Manifest Specificity] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> String
  makePetaRow manSys vls moos pers clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp = petarow where
    petarow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 2 vls)) ++ "\">" ++ parseAspect2 asp ++ "</th>" ++ terarows ++ "\n\t\t</tr>"
    terarows = concatMap (makeTeraRow manSys vls pers clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp) moos

  makeTeraRow :: ManifestSystem -> [Int] -> [Manifest Person] -> [Manifest Clusivity] -> [Manifest Definiteness] -> [Manifest Specificity] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> String
  makeTeraRow manSys vls pers clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo = terarow where
    terarow = "\n\t\t\t<tr>\n\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 3 vls)) ++ "\">" ++ parseMood2 moo ++ "</th>" ++ gigarows ++ "\n\t\t\t</tr>"
    gigarows = concatMap (makeGigaRow manSys vls clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo) pers

  makeGigaRow :: ManifestSystem -> [Int] -> [Manifest Clusivity] -> [Manifest Definiteness] -> [Manifest Specificity] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> String
  makeGigaRow manSys vls clus defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = gigarow where
    gigarow = "\n\t\t\t\t<tr>\n\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 4 vls)) ++ "\">" ++ parsePerson2 per ++ "</th>" ++ megarows ++ "\n\t\t\t\t</tr>"
    megarows = concatMap (makeMegaRow manSys vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per) clus

  makeMegaRow :: ManifestSystem -> [Int] -> [Manifest Definiteness] -> [Manifest Specificity] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> String
  makeMegaRow manSys vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per clu = megarow where
    megarow = "\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 5 vls)) ++ "\">" ++ parseClusivity2 clu ++ "</th>" ++ kilorows ++ "\n\t\t\t\t\t</tr>"
    kilorows = concatMap (makeKiloRow manSys vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per clu) defs

  makeKiloRow :: ManifestSystem -> [Int] -> [Manifest Specificity] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> String
  makeKiloRow manSys vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per clu def = kilorow where
    kilorow = "\n\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 6 vls)) ++ "\">" ++ parseDefiniteness2 def ++ "</th>" ++ hectorows ++ "\n\t\t\t\t\t\t</tr>"
    hectorows = concatMap (makeHectoRow manSys vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per clu def) spes

  makeHectoRow :: ManifestSystem -> [Int] -> [Manifest Polarity] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> String
  makeHectoRow manSys vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per clu def spe = hectorow where
    hectorow = "\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 7 vls)) ++ "\">" ++ parseSpecificity2 spe ++ "</th>" ++ decarows ++ "\n\t\t\t\t\t\t\t</tr>"
    decarows = concatMap (makeDecaRow manSys vls tops cass gens anis nums hons tras evis vois vols ten asp moo per clu def spe) pols

  makeDecaRow :: ManifestSystem -> [Int] -> [Manifest Topic] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> String
  makeDecaRow manSys vls tops cass gens anis nums hons tras evis vois vols ten asp moo per clu def spe pol = decarow where
    decarow = "\n\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 8 vls)) ++ "\">" ++ parsePolarity2 pol ++ "</th>" ++ rows ++ "\n\t\t\t\t\t\t\t\t</tr>"
    rows = concatMap (makeRow manSys vls cass gens anis nums hons tras evis vois vols ten asp moo per clu def spe pol) tops

  makeRow :: ManifestSystem -> [Int] -> [Manifest Case] -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> String
  makeRow manSys vls cass gens anis nums hons tras evis vois vols ten asp moo per clu def spe pol top = row where
    row = "\n\t\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 9 vls)) ++ "\">" ++ parseTopic2 top ++ "</th>" ++ decirows ++ "\n\t\t\t\t\t\t\t\t\t</tr>"
    decirows = concatMap (makeDeciRow manSys gens anis nums hons tras evis vois vols ten asp moo per clu def spe pol top) cass

  makeDeciRow :: ManifestSystem -> [Manifest Gender] -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> String
  makeDeciRow manSys gens anis nums hons tras evis vois vols ten asp moo per clu def spe pol top cas = concatMap (makeCentiRow manSys anis nums hons tras evis vois vols ten asp moo per clu def spe pol top cas) gens

  makeCentiRow :: ManifestSystem -> [Manifest Animacy] -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> String
  makeCentiRow manSys anis nums hons tras evis vois vols ten asp moo per clu def spe pol top cas gen = concatMap (makeMilliRow manSys nums hons tras evis vois vols ten asp moo per clu def spe pol top cas gen) anis

  makeMilliRow :: ManifestSystem -> [Manifest Number] -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> String
  makeMilliRow manSys nums hons tras evis vois vols ten asp moo per clu def spe pol top cas gen ani = concatMap (makeMicroRow manSys hons tras evis vois vols ten asp moo per clu def spe pol top cas gen ani) nums

  makeMicroRow :: ManifestSystem -> [Manifest Honorific] -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> Manifest Number -> String
  makeMicroRow manSys hons tras evis vois vols ten asp moo per clu def spe pol top cas gen ani num = concatMap (makeNanoRow manSys tras evis vois vols ten asp moo per clu def spe pol top cas gen ani num) hons

  makeNanoRow :: ManifestSystem -> [Manifest Transitivity] -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> Manifest Number -> Manifest Honorific -> String
  makeNanoRow manSys tras evis vois vols ten asp moo per clu def spe pol top cas gen ani num hon = concatMap (makePicoRow manSys evis vois vols ten asp moo per clu def spe pol top cas gen ani num hon) tras

  makePicoRow :: ManifestSystem -> [Manifest Evidentiality] -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> Manifest Number -> Manifest Honorific -> Manifest Transitivity -> String
  makePicoRow manSys evis vois vols ten asp moo per clu def spe pol top cas gen ani num hon tra = concatMap (makeFemtoRow manSys vois vols ten asp moo per clu def spe pol top cas gen ani num hon tra) evis

  makeFemtoRow :: ManifestSystem -> [Manifest Voice] -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> Manifest Number -> Manifest Honorific -> Manifest Transitivity -> Manifest Evidentiality -> String
  makeFemtoRow manSys vois vols ten asp moo per clu def spe pol top cas gen ani num hon tra evi = concatMap (makeAttoRow manSys vols ten asp moo per clu def spe pol top cas gen ani num hon tra evi) vois

  makeAttoRow :: ManifestSystem -> [Manifest Volition] -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> Manifest Number -> Manifest Honorific -> Manifest Transitivity -> Manifest Evidentiality -> Manifest Voice -> String
  makeAttoRow manSys vols ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi = cluster where
    cluster = "\n\t\t\t\t\t\t\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t\t\t\t\t\t\t<td>" (map (getMorpheme manSys ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi) vols) ++ "</td>"

  getMorpheme :: ManifestSystem -> Manifest Tense -> Manifest Aspect -> Manifest Mood -> Manifest Person -> Manifest Clusivity -> Manifest Definiteness -> Manifest Specificity -> Manifest Polarity -> Manifest Topic -> Manifest Case -> Manifest Gender -> Manifest Animacy -> Manifest Number -> Manifest Honorific -> Manifest Transitivity -> Manifest Evidentiality -> Manifest Voice -> Manifest Volition -> String
  getMorpheme (ManifestSystem _ PreParticle combos) ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | not.null $ filt = parseMorpheme (fst $ head filt)
      | otherwise = ""
  getMorpheme (ManifestSystem lc PostParticle combos) ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi vol = getMorpheme (ManifestSystem lc PreParticle combos) ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi vol
  getMorpheme (ManifestSystem _ Prefix combos) ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | not.null $ filt = parseMorpheme (fst $ head filt) ++ "–"
      | otherwise = ""
  getMorpheme (ManifestSystem _ Suffix combos) ten asp moo per clu def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,clu,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
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

parseClusivities :: Manifest [Clusivity] -> String
parseClusivities (Manifest _ clus) = "Clusivity (" ++ intercalate ", " (init strs) ++ ", and " ++ last strs ++ ")" where
  strs = map parseClusivity clus
parseClusivities NoManifest = ""

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
  | gen == M   = "Masculine"
  | gen == F   = "Feminine"
  | gen == COM = "Common"
  | gen == N   = "Neuter"

parseAnimacy :: Animacy -> String
parseAnimacy ani
  | ani == AN   = "Animate"
  | ani == HUM  = "Human"
  | ani == NHUM = "Non-Human"
  | ani == ZO   = "Animal"
  | ani == INAN = "Inanimate"

parseCase :: Case -> String
parseCase cas
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
  | num == SG  = "Singular"
  | num == DU  = "Dual"
  | num == TRI = "Trial"
  | num == PA  = "Paucal"
  | num == PL  = "Plural"

parseDefiniteness :: Definiteness -> String
parseDefiniteness def
  | def == DEF  = "Definite"
  | def == INDF = "Indefinite"

parseSpecificity :: Specificity -> String
parseSpecificity spe
  | spe == SPEC  = "Specific"
  | spe == NSPEC = "Nonspecific"

parseTopic :: Topic -> String
parseTopic top
  | top == TOP  = "Topic"
  | top == NTOP = "Not topic"

parsePerson :: Person -> String
parsePerson per
  | per == FIRST  = "First"
  | per == SECOND = "Second"
  | per == THIRD  = "Third"

parseClusivity :: Clusivity -> String
parseClusivity top
  | top == INCL = "Inclusive"
  | top == EXCL = "Exclusive"

parseHonorific :: Honorific -> String
parseHonorific hon
  | hon == FAM   = "Informal"
  | hon == FORM  = "Formal"

parsePolarity :: Polarity -> String
parsePolarity pol
  | pol == AFF = "Affirmative"
  | pol == NEG = "Negative"

parseTense :: Tense -> String
parseTense ten
  | ten == PST  = "Past"
  | ten == REM  = "Remote past"
  | ten == REC  = "Recent past"
  | ten == NPST = "Non-past"
  | ten == PRS  = "Present"
  | ten == NFUT = "Non-future"
  | ten == FUT  = "Future"
  | ten == IMMF = "Immediate future"
  | ten == REMF = "Remote future"

parseAspect :: Aspect -> String
parseAspect asp
  | asp == PFV  = "Perfect"
  | asp == IPFV = "Imperfect"

parseMood :: Mood -> String
parseMood moo
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
  | voi == ACTIVE  = "Active"
  | voi == MIDDLE  = "Middle"
  | voi == PASSIVE = "Passive"

parseEvidentiality :: Evidentiality -> String
parseEvidentiality evi
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
  | tra == NTRANS  = "Intransitive"
  | tra == TRANS   = "Transitive"
  | tra == DITRANS = "Ditransitive"

parseVolition :: Volition -> String
parseVolition vol
  | vol == VOL  = "Intended"
  | vol == NVOL = "Unintended"

parseGender2 :: Manifest Gender -> String
parseGender2 NoManifest = ""
parseGender2 (Manifest _ gen) = parseGender gen

parseAnimacy2 :: Manifest Animacy -> String
parseAnimacy2 NoManifest = ""
parseAnimacy2 (Manifest _ ani) = parseAnimacy ani

parseCase2 :: Manifest Case -> String
parseCase2 NoManifest = ""
parseCase2 (Manifest _ cas) = parseCase cas

parseNumber2 :: Manifest Number -> String
parseNumber2 NoManifest = ""
parseNumber2 (Manifest _ num) = parseNumber num

parseDefiniteness2 :: Manifest Definiteness -> String
parseDefiniteness2 NoManifest = ""
parseDefiniteness2 (Manifest _ def) = parseDefiniteness def

parseSpecificity2 :: Manifest Specificity -> String
parseSpecificity2 NoManifest = ""
parseSpecificity2 (Manifest _ spe) = parseSpecificity spe

parseTopic2 :: Manifest Topic -> String
parseTopic2 NoManifest = ""
parseTopic2 (Manifest _ top) = parseTopic top

parsePerson2 :: Manifest Person -> String
parsePerson2 NoManifest = ""
parsePerson2 (Manifest _ per) = parsePerson per

parseClusivity2 :: Manifest Clusivity -> String
parseClusivity2 NoManifest = ""
parseClusivity2 (Manifest _ clu) = parseClusivity clu

parseHonorific2 :: Manifest Honorific -> String
parseHonorific2 NoManifest = ""
parseHonorific2 (Manifest _ hon) = parseHonorific hon

parsePolarity2 :: Manifest Polarity -> String
parsePolarity2 NoManifest = ""
parsePolarity2 (Manifest _ pol) = parsePolarity pol

parseTense2 :: Manifest Tense -> String
parseTense2 NoManifest = ""
parseTense2 (Manifest _ ten) = parseTense ten

parseAspect2 :: Manifest Aspect -> String
parseAspect2 NoManifest = ""
parseAspect2 (Manifest _ asp) = parseAspect asp

parseMood2 :: Manifest Mood -> String
parseMood2 NoManifest = ""
parseMood2 (Manifest _ moo) = parseMood moo

parseVoice2 :: Manifest Voice -> String
parseVoice2 NoManifest = ""
parseVoice2 (Manifest _ voi) = parseVoice voi

parseEvidentiality2 :: Manifest Evidentiality -> String
parseEvidentiality2 NoManifest = ""
parseEvidentiality2 (Manifest _ evi) = parseEvidentiality evi

parseTransitivity2 :: Manifest Transitivity -> String
parseTransitivity2 NoManifest = ""
parseTransitivity2 (Manifest _ tra) = parseTransitivity tra

parseVolition2 :: Manifest Volition -> String
parseVolition2 NoManifest = ""
parseVolition2 (Manifest _ vol) = parseVolition vol


parseLexicalCategory :: LexicalCategory -> String
parseLexicalCategory lc
  | lc == Sub = "Subject"
  | lc == Obj = "Object"
  | lc == Adj = "Adjective"
  | lc == Adv = "Adverb"
  | lc == Prep = "Preposition"
  | lc == Verb ="Verb"
