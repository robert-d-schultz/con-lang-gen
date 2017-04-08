{-# LANGUAGE Rank2Types #-}
module Out.Inflection
( parseLCInflection
, parseLexicalSystems
) where

import Prelude hiding (Word)
import Data.List

import Data.Phoneme
import Data.Inflection
import Gen.Morphology
import Out.Lexicon

-- Parse inflection system (per lex cat)
parseLCInflection :: InflectionMap -> String
parseLCInflection inflSys = concatMap (parseInflectionMap inflSys) [Noun, Adj, Adv, Adpo, Verb]

-- Parse inflection system (gives summary)
parseInflectionMap :: InflectionMap -> LexCat -> String
parseInflectionMap inflSys lc = output where
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

  fooBar :: (forall a . Manifest a -> Bool) -> InflectionMap -> [String]
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


-- parse inflections into tables
-- tables organized by lexical category and manifest type
parseLexicalSystems :: InflectionMap -> [[Phoneme]] -> [ManifestSystem] -> String
parseLexicalSystems inflSys sonHier infls = "<br>\n" ++ concatMap (parseLexicalSystems_ inflSys sonHier infls) [Noun, Adj, Verb, Adv]


parseLexicalSystems_ :: InflectionMap -> [[Phoneme]] -> [ManifestSystem] -> LexCat -> String
parseLexicalSystems_ inflSys sonHier infls lc = "<br>\n" ++ parseLexCat lc
                                                         ++ parseManifestSystems parts sonHier (length parts) inflSys
                                                         ++ parseManifestSystems prefs sonHier (length prefs) inflSys
                                                         ++ parseManifestSystems suffs sonHier (length suffs) inflSys where
  parts = filter (\x -> manSysType x == Particle && manSysLC x == lc) infls
  prefs = filter (\x -> manSysType x == Prefix && manSysLC x == lc) infls
  suffs = filter (\x -> manSysType x == Suffix && manSysLC x == lc) infls

parseManifestSystems :: [ManifestSystem] -> [[Phoneme]] -> Int -> InflectionMap -> String
parseManifestSystems _ _ 0 _ = ""
parseManifestSystems [] _ _ _ = ""
parseManifestSystems expSyss sonHier i gramSys = parseManifestSystems (init expSyss) sonHier (i-1) gramSys ++ parseManifestSystem (last expSyss) sonHier gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol where
  ManifestSystem lc mt xs = last expSyss
  (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc mt i


-- Parse a manifestation system (particles/declensions) into an html table
-- Horizontal: Case, Gender, Animacy, Number, Honorific, Transitivity, Evidentiality, Voice, Volition (9)
-- Vertical:   Tense, Aspect, Mood, Person, Clusivity, Definiteness, Specificity, Polarity, Topic     (9)
parseManifestSystem :: ManifestSystem -> [[Phoneme]] -> [Express Gender] -> [Express Animacy] -> [Express Case] -> [Express Number] -> [Express Definiteness] -> [Express Specificity] -> [Express Topic] -> [Express Person] -> [Express Honorific] -> [Express Polarity] -> [Express Tense] -> [Express Aspect] -> [Express Mood] -> [Express Voice] -> [Express Evidentiality] -> [Express Transitivity] -> [Express Volition] -> String
parseManifestSystem manSys sonHier gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols  = "<br>\n<table border=1>" ++ title ++ header ++ exarows ++ "\n</table>\n" where

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
  makeHeader [] _ = ""
  makeHeader lss hls = makeLabel (length lss) (head lss) hls ++ makeHeader (tail lss) hls

  makeLabel :: Int -> [String] -> [Int] -> String
  makeLabel i ls hls = "\n\t<tr>\n\t\t<th colspan=\"9\">"
                 ++ concat (replicate (product $ take (9-i) hls) ("</th>\n\t\t<th colspan=\"" ++ show (product $ drop (10-i) hls) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ show (product $ drop (10-i) hls) ++ "\">") ls))
                 ++ "</th>\n\t"
                 ++ "</tr>"

  -- mega rows
  vls = [length tens,length asps,length moos,length pers,1,length defs,length spes,length pols,length tops]

  exarows = concatMap (makeExaRow manSys sonHier vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols) tens

  rowSpanN :: [Int] -> Int
  rowSpanN [] = 1
  rowSpanN ns =  head ns * rowSpanN (tail ns) + 1

  makeExaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Aspect] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> String
  makeExaRow manSys sonHier vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten = exarow where
    exarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ show (rowSpanN (drop 1 vls)) ++ "\">" ++ parseTense2 ten ++ "</th>" ++ petarows ++ "\n\t</tr>"
    petarows = concatMap (makePetaRow manSys sonHier vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten) asps

  makePetaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> String
  makePetaRow manSys sonHier vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp = petarow where
    petarow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 2 vls)) ++ "\">" ++ parseAspect2 asp ++ "</th>" ++ terarows ++ "\n\t\t</tr>"
    terarows = concatMap (makeTeraRow manSys sonHier vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp) moos

  makeTeraRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> String
  makeTeraRow manSys sonHier vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo = terarow where
    terarow = "\n\t\t\t<tr>\n\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 3 vls)) ++ "\">" ++ parseMood2 moo ++ "</th>" ++ gigarows ++ "\n\t\t\t</tr>"
    gigarows = concatMap (makeGigaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo) pers

  makeGigaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> String
  makeGigaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = gigarow where
    gigarow = "\n\t\t\t\t<tr>\n\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 4 vls)) ++ "\">" ++ parsePerson2 per ++ "</th>" ++ megarows ++ "\n\t\t\t\t</tr>"
    megarows = makeMegaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per

  makeMegaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> String
  makeMegaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = megarow where
    megarow = "\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 5 vls)) ++ "\">" ++ "</th>" ++ kilorows ++ "\n\t\t\t\t\t</tr>"
    kilorows = concatMap (makeKiloRow manSys sonHier vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per) defs

  makeKiloRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> String
  makeKiloRow manSys sonHier vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def = kilorow where
    kilorow = "\n\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 6 vls)) ++ "\">" ++ parseDefiniteness2 def ++ "</th>" ++ hectorows ++ "\n\t\t\t\t\t\t</tr>"
    hectorows = concatMap (makeHectoRow manSys sonHier vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def) spes

  makeHectoRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> String
  makeHectoRow manSys sonHier vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe = hectorow where
    hectorow = "\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 7 vls)) ++ "\">" ++ parseSpecificity2 spe ++ "</th>" ++ decarows ++ "\n\t\t\t\t\t\t\t</tr>"
    decarows = concatMap (makeDecaRow manSys sonHier vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe) pols

  makeDecaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> String
  makeDecaRow manSys sonHier vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol = decarow where
    decarow = "\n\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 8 vls)) ++ "\">" ++ parsePolarity2 pol ++ "</th>" ++ rows ++ "\n\t\t\t\t\t\t\t\t</tr>"
    rows = concatMap (makeRow manSys sonHier vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol) tops

  makeRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> String
  makeRow manSys sonHier vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol top = row where
    row = "\n\t\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ show (rowSpanN (drop 9 vls)) ++ "\">" ++ parseTopic2 top ++ "</th>" ++ decirows ++ "\n\t\t\t\t\t\t\t\t\t</tr>"
    decirows = concatMap (makeDeciRow manSys sonHier gens anis nums hons tras evis vois vols ten asp moo per def spe pol top) cass

  makeDeciRow :: ManifestSystem -> [[Phoneme]] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> String
  makeDeciRow manSys sonHier gens anis nums hons tras evis vois vols ten asp moo per def spe pol top cas = concatMap (makeCentiRow manSys sonHier anis nums hons tras evis vois vols ten asp moo per def spe pol top cas) gens

  makeCentiRow :: ManifestSystem -> [[Phoneme]] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> String
  makeCentiRow manSys sonHier anis nums hons tras evis vois vols ten asp moo per def spe pol top cas gen = concatMap (makeMilliRow manSys sonHier nums hons tras evis vois vols ten asp moo per def spe pol top cas gen) anis

  makeMilliRow :: ManifestSystem -> [[Phoneme]] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> String
  makeMilliRow manSys sonHier nums hons tras evis vois vols ten asp moo per def spe pol top cas gen ani = concatMap (makeMicroRow manSys sonHier hons tras evis vois vols ten asp moo per def spe pol top cas gen ani) nums

  makeMicroRow :: ManifestSystem -> [[Phoneme]] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> String
  makeMicroRow manSys sonHier hons tras evis vois vols ten asp moo per def spe pol top cas gen ani num = concatMap (makeNanoRow manSys sonHier tras evis vois vols ten asp moo per def spe pol top cas gen ani num) hons

  makeNanoRow :: ManifestSystem -> [[Phoneme]] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> String
  makeNanoRow manSys sonHier tras evis vois vols ten asp moo per def spe pol top cas gen ani num hon = concatMap (makePicoRow manSys sonHier evis vois vols ten asp moo per def spe pol top cas gen ani num hon) tras

  makePicoRow :: ManifestSystem -> [[Phoneme]] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> String
  makePicoRow manSys sonHier evis vois vols ten asp moo per def spe pol top cas gen ani num hon tra = concatMap (makeFemtoRow manSys sonHier vois vols ten asp moo per def spe pol top cas gen ani num hon tra) evis

  makeFemtoRow :: ManifestSystem -> [[Phoneme]] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> String
  makeFemtoRow manSys sonHier vois vols ten asp moo per def spe pol top cas gen ani num hon tra evi = concatMap (makeAttoRow manSys sonHier vols ten asp moo per def spe pol top cas gen ani num hon tra evi) vois

  makeAttoRow :: ManifestSystem -> [[Phoneme]] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> String
  makeAttoRow manSys sonHier vols ten asp moo per def spe pol top cas gen ani num hon tra evi voi = cluster where
    cluster = "\n\t\t\t\t\t\t\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t\t\t\t\t\t\t<td>" (map (getMorpheme manSys sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi) vols) ++ "</td>"

  getMorpheme :: ManifestSystem -> [[Phoneme]] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Express Volition -> String
  getMorpheme (ManifestSystem _ Particle combos) sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | null filt = ""
      | otherwise = parseMorphemeIPA sonHier (fst $ head filt)
  getMorpheme (ManifestSystem _ Prefix combos) sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | null filt = ""
      | otherwise = parseMorphemeIPA sonHier (fst $ head filt) ++ "–"
  getMorpheme (ManifestSystem _ Suffix combos) sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = output where
    filt = filter (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output
      | null filt = ""
      | otherwise = "–" ++ parseMorphemeIPA sonHier (fst $ head filt)

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
