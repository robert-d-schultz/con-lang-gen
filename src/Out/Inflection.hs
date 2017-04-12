{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Inflection
( parseLCInflection
, parseLexicalSystems
) where

import ClassyPrelude hiding (Word)
import Data.List as R (replicate)

import Data.Phoneme
import Data.Inflection
import Gen.Morphology
import Out.Lexicon

-- Parse inflection system (per lex cat)
parseLCInflection :: InflectionMap -> Text
parseLCInflection inflSys = concatMap (parseInflectionMap inflSys) [Noun, Adj, Adv, Adpo, Verb]

-- Parse inflection map (gives summary)
parseInflectionMap :: InflectionMap -> LexCat -> Text
parseInflectionMap inflSys lc = output where
  output
    | null (particles ++ prefixes ++ suffixes) = "<br>\nNo grammatical categories manifest for " ++ tshow lc ++ "s.\n"
    | otherwise = "<br>\nGrammatical categories manifest for " ++ tshow lc ++ "s in the following ways:\n<ul>" ++ particles ++ prefixes ++ suffixes ++ "</ul>\n"

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

  fooBar :: (forall a . Manifest a -> Bool) -> InflectionMap -> [Text]
  fooBar b inflSys = [gen, ani, cas, num, def, spe, top, per, hon, pol, ten, asp, moo, voi, evi, tra, vol] where
    gen | b (genSys inflSys) = "Gender: " ++ tshow (genSys inflSys)          | otherwise = ""
    ani | b (aniSys inflSys) = "Animacy: " ++ tshow (aniSys inflSys)         | otherwise = ""
    cas | b (casSys inflSys) = "Case: " ++ tshow (casSys inflSys)            | otherwise = ""
    num | b (numSys inflSys) = "Number: " ++ tshow (numSys inflSys)          | otherwise = ""
    def | b (defSys inflSys) = "Definitenesses: " ++ tshow (defSys inflSys)  | otherwise = ""
    spe | b (speSys inflSys) = "Specificities: " ++ tshow (speSys inflSys)   | otherwise = ""
    top | b (topSys inflSys) = "Topics: " ++ tshow (topSys inflSys)          | otherwise = ""
    per | b (perSys inflSys) = "Persons: " ++ tshow (perSys inflSys)         | otherwise = ""
    hon | b (honSys inflSys) = "Honorifics: " ++ tshow (honSys inflSys)      | otherwise = ""
    pol | b (polSys inflSys) = "Polarities: " ++ tshow (polSys inflSys)      | otherwise = ""
    ten | b (tenSys inflSys) = "Tenses: " ++ tshow (tenSys inflSys)          | otherwise = ""
    asp | b (aspSys inflSys) = "Aspects: " ++ tshow (aspSys inflSys)         | otherwise = ""
    moo | b (mooSys inflSys) = "Moods: " ++ tshow (mooSys inflSys)           | otherwise = ""
    voi | b (voiSys inflSys) = "Voices: " ++ tshow (voiSys inflSys)          | otherwise = ""
    evi | b (eviSys inflSys) = "Evidentialities: " ++ tshow (eviSys inflSys) | otherwise = ""
    tra | b (traSys inflSys) = "Transitivities: " ++ tshow (traSys inflSys)  | otherwise = ""
    vol | b (volSys inflSys) = "Volitions: " ++ tshow (volSys inflSys)       | otherwise = ""

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
parseLexicalSystems :: InflectionMap -> [[Phoneme]] -> [ManifestSystem] -> Text
parseLexicalSystems inflSys sonHier infls = "<br>\n" ++ concatMap (parseLexicalSystems_ inflSys sonHier infls) [Noun, Adj, Verb, Adv]


parseLexicalSystems_ :: InflectionMap -> [[Phoneme]] -> [ManifestSystem] -> LexCat -> Text
parseLexicalSystems_ inflSys sonHier infls lc = "<br>\n" ++ tshow lc
                                                         ++ parseManifestSystems parts sonHier (length parts) inflSys
                                                         ++ parseManifestSystems prefs sonHier (length prefs) inflSys
                                                         ++ parseManifestSystems suffs sonHier (length suffs) inflSys  where
  parts = filter (\x -> manSysType x == Particle && manSysLC x == lc) infls
  prefs = filter (\x -> manSysType x == Prefix && manSysLC x == lc) infls
  suffs = filter (\x -> manSysType x == Suffix && manSysLC x == lc) infls

parseManifestSystems :: [ManifestSystem] -> [[Phoneme]] -> Int -> InflectionMap -> Text
parseManifestSystems _ _ 0 _ = ""
parseManifestSystems [] _ _ _ = ""
parseManifestSystems expSyss sonHier i gramSys = fromMaybe "" out where
  out = do
    lst <- lastMay expSyss
    ini <- initMay expSyss
    let ManifestSystem lc mt xs = lst
    let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc mt i
    let txt = parseManifestSystems ini sonHier (i-1) gramSys ++ parseManifestSystem lst sonHier gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol
    return txt



-- Parse a manifestation system (particles/declensions) into an html table
-- Horizontal: Case, Gender, Animacy, Number, Honorific, Transitivity, Evidentiality, Voice, Volition (9)
-- Vertical:   Tense, Aspect, Mood, Person, Clusivity, Definiteness, Specificity, Polarity, Topic     (9)
parseManifestSystem :: ManifestSystem -> [[Phoneme]] -> [Express Gender] -> [Express Animacy] -> [Express Case] -> [Express Number] -> [Express Definiteness] -> [Express Specificity] -> [Express Topic] -> [Express Person] -> [Express Honorific] -> [Express Polarity] -> [Express Tense] -> [Express Aspect] -> [Express Mood] -> [Express Voice] -> [Express Evidentiality] -> [Express Transitivity] -> [Express Volition] -> Text
parseManifestSystem manSys sonHier gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols  = "<br>\n<table border=1>" ++ title ++ header ++ exarows ++ "\n</table>\n" where

  -- title
  hls = [length cass,length gens,length anis,length nums,length hons,length tras,length evis,length vois,length vols]
  title = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (product hls + 9) ++ "\">" ++ parseSystemType manSys ++ "</th>\n\t</tr>"


  parseSystemType :: ManifestSystem -> Text
  parseSystemType (ManifestSystem _ Particle _) = "Particle"
  parseSystemType (ManifestSystem _ Prefix _)   = "Prefix"
  parseSystemType (ManifestSystem _ Suffix _)   = "Suffix"

  -- header
  horLabels = [map tshow cass, map tshow gens, map tshow anis, map tshow nums, map tshow hons, map tshow tras, map tshow evis, map tshow vois, map tshow vols]
  header = makeHeader horLabels hls

  makeHeader :: [[Text]] -> [Int] -> Text
  makeHeader [] _ = ""
  makeHeader (ls:lss) hls = makeLabel (length lss + 1) ls hls ++ makeHeader lss hls

  makeLabel :: Int -> [Text] -> [Int] -> Text
  makeLabel i ls hls = "\n\t<tr>\n\t\t<th colspan=\"9\">"
                 ++ concat (R.replicate (product $ take (9-i) hls) ("</th>\n\t\t<th colspan=\"" ++ tshow (product $ drop (10-i) hls) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ tshow (product $ drop (10-i) hls) ++ "\">") ls))
                 ++ "</th>\n\t"
                 ++ "</tr>"

  -- mega rows
  vls = [length tens,length asps,length moos,length pers,1,length defs,length spes,length pols,length tops]

  exarows = concatMap (makeExaRow manSys sonHier vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols) tens

  rowSpanN :: [Int] -> Int
  rowSpanN [] = 1
  rowSpanN (n:ns) = n * rowSpanN ns + 1

  makeExaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Aspect] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Text
  makeExaRow manSys sonHier vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten = exarow where
    exarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 1 vls)) ++ "\">" ++ tshow ten ++ "</th>" ++ petarows ++ "\n\t</tr>"
    petarows = concatMap (makePetaRow manSys sonHier vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten) asps

  makePetaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Text
  makePetaRow manSys sonHier vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp = petarow where
    petarow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 2 vls)) ++ "\">" ++ tshow asp ++ "</th>" ++ terarows ++ "\n\t\t</tr>"
    terarows = concatMap (makeTeraRow manSys sonHier vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp) moos

  makeTeraRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Text
  makeTeraRow manSys sonHier vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo = terarow where
    terarow = "\n\t\t\t<tr>\n\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 3 vls)) ++ "\">" ++ tshow moo ++ "</th>" ++ gigarows ++ "\n\t\t\t</tr>"
    gigarows = concatMap (makeGigaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo) pers

  makeGigaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Text
  makeGigaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = gigarow where
    gigarow = "\n\t\t\t\t<tr>\n\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 4 vls)) ++ "\">" ++ tshow per ++ "</th>" ++ megarows ++ "\n\t\t\t\t</tr>"
    megarows = makeMegaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per

  makeMegaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Text
  makeMegaRow manSys sonHier vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = megarow where
    megarow = "\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 5 vls)) ++ "\">" ++ "</th>" ++ kilorows ++ "\n\t\t\t\t\t</tr>"
    kilorows = concatMap (makeKiloRow manSys sonHier vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per) defs

  makeKiloRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Text
  makeKiloRow manSys sonHier vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def = kilorow where
    kilorow = "\n\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 6 vls)) ++ "\">" ++ tshow def ++ "</th>" ++ hectorows ++ "\n\t\t\t\t\t\t</tr>"
    hectorows = concatMap (makeHectoRow manSys sonHier vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def) spes

  makeHectoRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Text
  makeHectoRow manSys sonHier vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe = hectorow where
    hectorow = "\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 7 vls)) ++ "\">" ++ tshow spe ++ "</th>" ++ decarows ++ "\n\t\t\t\t\t\t\t</tr>"
    decarows = concatMap (makeDecaRow manSys sonHier vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe) pols

  makeDecaRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Text
  makeDecaRow manSys sonHier vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol = decarow where
    decarow = "\n\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 8 vls)) ++ "\">" ++ tshow pol ++ "</th>" ++ rows ++ "\n\t\t\t\t\t\t\t\t</tr>"
    rows = concatMap (makeRow manSys sonHier vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol) tops

  makeRow :: ManifestSystem -> [[Phoneme]] -> [Int] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Text
  makeRow manSys sonHier vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol top = row where
    row = "\n\t\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 9 vls)) ++ "\">" ++ tshow top ++ "</th>" ++ decirows ++ "\n\t\t\t\t\t\t\t\t\t</tr>"
    decirows = concatMap (makeDeciRow manSys sonHier gens anis nums hons tras evis vois vols ten asp moo per def spe pol top) cass

  makeDeciRow :: ManifestSystem -> [[Phoneme]] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Text
  makeDeciRow manSys sonHier gens anis nums hons tras evis vois vols ten asp moo per def spe pol top cas = concatMap (makeCentiRow manSys sonHier anis nums hons tras evis vois vols ten asp moo per def spe pol top cas) gens

  makeCentiRow :: ManifestSystem -> [[Phoneme]] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Text
  makeCentiRow manSys sonHier anis nums hons tras evis vois vols ten asp moo per def spe pol top cas gen = concatMap (makeMilliRow manSys sonHier nums hons tras evis vois vols ten asp moo per def spe pol top cas gen) anis

  makeMilliRow :: ManifestSystem -> [[Phoneme]] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Text
  makeMilliRow manSys sonHier nums hons tras evis vois vols ten asp moo per def spe pol top cas gen ani = concatMap (makeMicroRow manSys sonHier hons tras evis vois vols ten asp moo per def spe pol top cas gen ani) nums

  makeMicroRow :: ManifestSystem -> [[Phoneme]] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Text
  makeMicroRow manSys sonHier hons tras evis vois vols ten asp moo per def spe pol top cas gen ani num = concatMap (makeNanoRow manSys sonHier tras evis vois vols ten asp moo per def spe pol top cas gen ani num) hons

  makeNanoRow :: ManifestSystem -> [[Phoneme]] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Text
  makeNanoRow manSys sonHier tras evis vois vols ten asp moo per def spe pol top cas gen ani num hon = concatMap (makePicoRow manSys sonHier evis vois vols ten asp moo per def spe pol top cas gen ani num hon) tras

  makePicoRow :: ManifestSystem -> [[Phoneme]] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Text
  makePicoRow manSys sonHier evis vois vols ten asp moo per def spe pol top cas gen ani num hon tra = concatMap (makeFemtoRow manSys sonHier vois vols ten asp moo per def spe pol top cas gen ani num hon tra) evis

  makeFemtoRow :: ManifestSystem -> [[Phoneme]] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Text
  makeFemtoRow manSys sonHier vois vols ten asp moo per def spe pol top cas gen ani num hon tra evi = concatMap (makeAttoRow manSys sonHier vols ten asp moo per def spe pol top cas gen ani num hon tra evi) vois

  makeAttoRow :: ManifestSystem -> [[Phoneme]] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Text
  makeAttoRow manSys sonHier vols ten asp moo per def spe pol top cas gen ani num hon tra evi voi = cluster where
    cluster = "\n\t\t\t\t\t\t\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t\t\t\t\t\t\t<td>" (map (getMorpheme manSys sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi) vols) ++ "</td>"

  getMorpheme :: ManifestSystem -> [[Phoneme]] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Express Volition -> Text
  getMorpheme (ManifestSystem _ Particle combos) sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
    filt = find (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output = parseMorphemeIPA sonHier <$> (fst <$> filt)
  getMorpheme (ManifestSystem _ Prefix combos) sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
    filt = find (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output = (++ "–") <$> (parseMorphemeIPA sonHier <$> (fst <$> filt))
  getMorpheme (ManifestSystem _ Suffix combos) sonHier ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
    filt = find (\(morph, sys) -> sys == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
    output = (++) "–" <$> (parseMorphemeIPA sonHier <$> (fst <$> filt))
