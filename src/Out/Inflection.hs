{-# LANGUAGE Rank2Types #-}
module Out.Inflection
( writeLCInflection
, writeLexicalSystems
) where

import ClassyPrelude
import Data.List as R (replicate)

import Data.Word
import Data.Inflection
import Data.Language

import Gen.Morphology
import Out.Lexicon

-- Write inflection system (per lex cat)
writeLCInflection :: InflectionMap -> Text
writeLCInflection inflSys = concatMap (writeInflectionMap inflSys) [Noun, Adj, Adv, Adpo, Verb]

-- Write inflection map (gives summary)
writeInflectionMap :: InflectionMap -> LexCat -> Text
writeInflectionMap inflSys lc = output where
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
  gen | b (getGenSys inflSys) = "Gender: " ++ tshow (getGenSys inflSys)          | otherwise = ""
  ani | b (getAniSys inflSys) = "Animacy: " ++ tshow (getAniSys inflSys)         | otherwise = ""
  cas | b (getCasSys inflSys) = "Case: " ++ tshow (getCasSys inflSys)            | otherwise = ""
  num | b (getNumSys inflSys) = "Number: " ++ tshow (getNumSys inflSys)          | otherwise = ""
  def | b (getDefSys inflSys) = "Definitenesses: " ++ tshow (getDefSys inflSys)  | otherwise = ""
  spe | b (getSpeSys inflSys) = "Specificities: " ++ tshow (getSpeSys inflSys)   | otherwise = ""
  top | b (getTopSys inflSys) = "Topics: " ++ tshow (getTopSys inflSys)          | otherwise = ""
  per | b (getPerSys inflSys) = "Persons: " ++ tshow (getPerSys inflSys)         | otherwise = ""
  hon | b (getHonSys inflSys) = "Honorifics: " ++ tshow (getHonSys inflSys)      | otherwise = ""
  pol | b (getPolSys inflSys) = "Polarities: " ++ tshow (getPolSys inflSys)      | otherwise = ""
  ten | b (getTenSys inflSys) = "Tenses: " ++ tshow (getTenSys inflSys)          | otherwise = ""
  asp | b (getAspSys inflSys) = "Aspects: " ++ tshow (getAspSys inflSys)         | otherwise = ""
  moo | b (getMooSys inflSys) = "Moods: " ++ tshow (getMooSys inflSys)           | otherwise = ""
  voi | b (getVoiSys inflSys) = "Voices: " ++ tshow (getVoiSys inflSys)          | otherwise = ""
  evi | b (getEviSys inflSys) = "Evidentialities: " ++ tshow (getEviSys inflSys) | otherwise = ""
  tra | b (getTraSys inflSys) = "Transitivities: " ++ tshow (getTraSys inflSys)  | otherwise = ""
  vol | b (getVolSys inflSys) = "Volitions: " ++ tshow (getVolSys inflSys)       | otherwise = ""

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


-- Write inflections to tables
-- tables organized by lexical category and manifest type
writeLexicalSystems :: InflectionMap -> Language -> [ManifestSystem] -> Text
writeLexicalSystems inflSys lang infls = "<br>\n" ++ style ++ concatMap (writeLexicalSystems_ inflSys lang infls) [Noun, Adj, Verb, Adv] where
  style = "<style>table{border-collapse:collapse;}th,td{empty-cells:hide;border:solid 1px black;padding:4px 4px;}th:empty,td:empty,tr:empty{border:0px;padding:0px 0px;}</style>\n"

writeLexicalSystems_ :: InflectionMap -> Language -> [ManifestSystem] -> LexCat -> Text
writeLexicalSystems_ inflSys lang infls lc = "<br>\n" ++ tshow lc
                                                         ++ writeManifestSystems parts lang (length parts) inflSys
                                                         ++ writeManifestSystems prefs lang (length prefs) inflSys
                                                         ++ writeManifestSystems suffs lang (length suffs) inflSys  where
  parts = filter (\x -> manSysType x == Particle && manSysLC x == lc) infls
  prefs = filter (\x -> manSysType x == Prefix && manSysLC x == lc) infls
  suffs = filter (\x -> manSysType x == Suffix && manSysLC x == lc) infls

writeManifestSystems :: [ManifestSystem] -> Language -> Int -> InflectionMap -> Text
writeManifestSystems _ _ 0 _ = ""
writeManifestSystems [] _ _ _ = ""
writeManifestSystems expSyss lang i gramSys = fromMaybe "" out where
  out = do
    lst <- lastMay expSyss
    ini <- initMay expSyss
    let ManifestSystem lc mt _ = lst
    let (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol) = cleanInflectionSys gramSys lc mt i
    let txt = writeManifestSystems ini lang (i-1) gramSys ++ writeManifestSystem lst lang gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol
    return txt



--Write a manifestation system (particles/declensions) into an html table
-- Horizontal: Case, Gender, Animacy, Number, Honorific, Transitivity, Evidentiality, Voice, Volition (9)
-- Vertical:   Tense, Aspect, Mood, Person, Clusivity, Definiteness, Specificity, Polarity, Topic     (9)
writeManifestSystem :: ManifestSystem -> Language -> [Express Gender] -> [Express Animacy] -> [Express Case] -> [Express Number] -> [Express Definiteness] -> [Express Specificity] -> [Express Topic] -> [Express Person] -> [Express Honorific] -> [Express Polarity] -> [Express Tense] -> [Express Aspect] -> [Express Mood] -> [Express Voice] -> [Express Evidentiality] -> [Express Transitivity] -> [Express Volition] -> Text
writeManifestSystem manSys lang gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols  = "<br>\n<table>" ++ title ++ header ++ exarows ++ "\n</table>\n" where

  -- title
  hls = [length cass,length gens,length anis,length nums,length hons,length tras,length evis,length vois,length vols]
  title = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (product hls + 9) ++ "\">" ++ writeSystemType manSys ++ "</th>\n\t</tr>"


  writeSystemType :: ManifestSystem -> Text
  writeSystemType (ManifestSystem _ Particle _) = "Particle"
  writeSystemType (ManifestSystem _ Prefix _)   = "Prefix"
  writeSystemType (ManifestSystem _ Suffix _)   = "Suffix"

  -- header
  horLabels = [map tshow cass, map tshow gens, map tshow anis, map tshow nums, map tshow hons, map tshow tras, map tshow evis, map tshow vois, map tshow vols]
  header = makeHeader horLabels hls

  -- mega rows
  vls = [length tens,length asps,length moos,length pers,1,length defs,length spes,length pols,length tops]

  exarows = concatMap (makeExaRow manSys lang vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols) tens

makeHeader :: [[Text]] -> [Int] -> Text
makeHeader [] _ = ""
makeHeader (ls:lss) hls = makeLabel (length lss + 1) ls hls ++ makeHeader lss hls

makeLabel :: Int -> [Text] -> [Int] -> Text
makeLabel i ls hls = "\n\t<tr>\n\t\t<th colspan=\"9\">"
               ++ concat (R.replicate (product $ take (9-i) hls) ("</th>\n\t\t<th colspan=\"" ++ tshow (product $ drop (10-i) hls) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ tshow (product $ drop (10-i) hls) ++ "\">") ls))
               ++ "</th>\n\t"
               ++ "</tr>"

rowSpanN :: [Int] -> Int
rowSpanN [] = 1
rowSpanN (n:ns) = n * rowSpanN ns + 1

makeExaRow :: ManifestSystem -> Language -> [Int] -> [Express Aspect] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Text
makeExaRow manSys lang vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten = exarow where
  exarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 1 vls)) ++ "\">" ++ tshow ten ++ "</th>" ++ petarows ++ "\n\t</tr>"
  petarows = concatMap (makePetaRow manSys lang vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten) asps

makePetaRow :: ManifestSystem -> Language -> [Int] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Text
makePetaRow manSys lang vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp = petarow where
  petarow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 2 vls)) ++ "\">" ++ tshow asp ++ "</th>" ++ terarows ++ "\n\t\t</tr>"
  terarows = concatMap (makeTeraRow manSys lang vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp) moos

makeTeraRow :: ManifestSystem -> Language -> [Int] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Text
makeTeraRow manSys lang vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo = terarow where
  terarow = "\n\t\t\t<tr>\n\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 3 vls)) ++ "\">" ++ tshow moo ++ "</th>" ++ gigarows ++ "\n\t\t\t</tr>"
  gigarows = concatMap (makeGigaRow manSys lang vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo) pers

makeGigaRow :: ManifestSystem -> Language -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Text
makeGigaRow manSys lang vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = gigarow where
  gigarow = "\n\t\t\t\t<tr>\n\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 4 vls)) ++ "\">" ++ tshow per ++ "</th>" ++ megarows ++ "\n\t\t\t\t</tr>"
  megarows = makeMegaRow manSys lang vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per

makeMegaRow :: ManifestSystem -> Language -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Text
makeMegaRow manSys lang vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = megarow where
  megarow = "\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 5 vls)) ++ "\">" ++ "</th>" ++ kilorows ++ "\n\t\t\t\t\t</tr>"
  kilorows = concatMap (makeKiloRow manSys lang vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per) defs

makeKiloRow :: ManifestSystem -> Language -> [Int] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Text
makeKiloRow manSys lang vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def = kilorow where
  kilorow = "\n\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 6 vls)) ++ "\">" ++ tshow def ++ "</th>" ++ hectorows ++ "\n\t\t\t\t\t\t</tr>"
  hectorows = concatMap (makeHectoRow manSys lang vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def) spes

makeHectoRow :: ManifestSystem -> Language -> [Int] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Text
makeHectoRow manSys lang vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe = hectorow where
  hectorow = "\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 7 vls)) ++ "\">" ++ tshow spe ++ "</th>" ++ decarows ++ "\n\t\t\t\t\t\t\t</tr>"
  decarows = concatMap (makeDecaRow manSys lang vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe) pols

makeDecaRow :: ManifestSystem -> Language -> [Int] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Text
makeDecaRow manSys lang vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol = decarow where
  decarow = "\n\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 8 vls)) ++ "\">" ++ tshow pol ++ "</th>" ++ rows ++ "\n\t\t\t\t\t\t\t\t</tr>"
  rows = concatMap (makeRow manSys lang vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol) tops

makeRow :: ManifestSystem -> Language -> [Int] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Text
makeRow manSys lang vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol top = row where
  row = "\n\t\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 9 vls)) ++ "\">" ++ tshow top ++ "</th>" ++ decirows ++ "\n\t\t\t\t\t\t\t\t\t</tr>"
  decirows = concatMap (makeDeciRow manSys lang gens anis nums hons tras evis vois vols ten asp moo per def spe pol top) cass

makeDeciRow :: ManifestSystem -> Language -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Text
makeDeciRow manSys lang gens anis nums hons tras evis vois vols ten asp moo per def spe pol top cas = concatMap (makeCentiRow manSys lang anis nums hons tras evis vois vols ten asp moo per def spe pol top cas) gens

makeCentiRow :: ManifestSystem -> Language -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Text
makeCentiRow manSys lang anis nums hons tras evis vois vols ten asp moo per def spe pol top cas gen = concatMap (makeMilliRow manSys lang nums hons tras evis vois vols ten asp moo per def spe pol top cas gen) anis

makeMilliRow :: ManifestSystem -> Language -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Text
makeMilliRow manSys lang nums hons tras evis vois vols ten asp moo per def spe pol top cas gen ani = concatMap (makeMicroRow manSys lang hons tras evis vois vols ten asp moo per def spe pol top cas gen ani) nums

makeMicroRow :: ManifestSystem -> Language -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Text
makeMicroRow manSys lang hons tras evis vois vols ten asp moo per def spe pol top cas gen ani num = concatMap (makeNanoRow manSys lang tras evis vois vols ten asp moo per def spe pol top cas gen ani num) hons

makeNanoRow :: ManifestSystem -> Language -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Text
makeNanoRow manSys lang tras evis vois vols ten asp moo per def spe pol top cas gen ani num hon = concatMap (makePicoRow manSys lang evis vois vols ten asp moo per def spe pol top cas gen ani num hon) tras

makePicoRow :: ManifestSystem -> Language -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Text
makePicoRow manSys lang evis vois vols ten asp moo per def spe pol top cas gen ani num hon tra = concatMap (makeFemtoRow manSys lang vois vols ten asp moo per def spe pol top cas gen ani num hon tra) evis

makeFemtoRow :: ManifestSystem -> Language -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Text
makeFemtoRow manSys lang vois vols ten asp moo per def spe pol top cas gen ani num hon tra evi = concatMap (makeAttoRow manSys lang vols ten asp moo per def spe pol top cas gen ani num hon tra evi) vois

makeAttoRow :: ManifestSystem -> Language -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Text
makeAttoRow manSys lang vols ten asp moo per def spe pol top cas gen ani num hon tra evi voi = cluster where
  cluster = "\n\t\t\t\t\t\t\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t\t\t\t\t\t\t<td>" (map (getMorpheme manSys lang ten asp moo per def spe pol top cas gen ani num hon tra evi voi) vols) ++ "</td>"

getMorpheme :: ManifestSystem -> Language -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Express Volition -> Text
getMorpheme (ManifestSystem _ Particle combos) lang ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
  filt = find (\morph -> getAllExpress (getMeaning morph) == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
  output = writeMorphemeIPA lang <$> filt
getMorpheme (ManifestSystem _ Prefix combos) lang ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
  filt = find (\morph -> getAllExpress (getMeaning morph) == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
  output = (++ "–") <$> (writeMorphemeIPA lang <$> filt)
getMorpheme (ManifestSystem _ Suffix combos) lang ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
  filt = find (\morph -> getAllExpress (getMeaning morph) == (gen,ani,cas,num,def,spe,top,per,hon,pol,ten,asp,moo,voi,evi,tra,vol)) combos
  output = (++) "–" <$> (writeMorphemeIPA lang <$> filt)
