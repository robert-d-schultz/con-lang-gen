{-# LANGUAGE Rank2Types #-}
module Out.Inflection
( writeInflectionOverview
, writeInflectionTables
) where

import ClassyPrelude
import Data.List as R (replicate)

import Data.Word
import Data.Inflection
import Data.Language

import Gen.Morpheme
import Out.Lexicon

-- Write inflection overview (per lex cat)
writeInflectionOverview :: InflectionMap -> Text
writeInflectionOverview inflSys = concatMap (writeInflectionMap inflSys) [Verb .. Pron]

-- Write inflection map (gives summary)
writeInflectionMap :: InflectionMap -> LexCat -> Text
writeInflectionMap inflSys lc = output where
  output
    | null (particles ++ prefixes ++ suffixes ++ transfixes) = "<br>\nNo grammatical categories manifest for " ++ tshow lc ++ "s.\n"
    | otherwise = "<br>\nGrammatical categories manifest for " ++ tshow lc ++ "s in the following ways:\n<ul>" ++ particles ++ prefixes ++ suffixes ++ transfixes ++ ctransfixes ++ "</ul>\n"

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

  -- parse transfixes
  filt4 = filter (not.null) (fooBar (isTransfix lc) inflSys)
  transfixes
    | null filt4      = ""
    | otherwise       = "\n\t<li>With vowel transfixes:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt4 ++ "</li>\n\t\t</ul>"

  -- parse transfixes
  filt5 = filter (not.null) (fooBar (isCTransfix lc) inflSys)
  ctransfixes
    | null filt5      = ""
    | otherwise       = "\n\t<li>With consonant transfixes:</li>\n\t\t<ul>\n\t\t\t<li>" ++ intercalate "</li>\n\t\t\t<li>" filt5 ++ "</li>\n\t\t</ul>"

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

isTransfix :: LexCat -> Manifest a -> Bool
isTransfix _ NoManifest = False
isTransfix lc (Manifest t _) = out where
  out
    | null filt = False
    | otherwise = True
  filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == Transfix) t

isCTransfix :: LexCat -> Manifest a -> Bool
isCTransfix _ NoManifest = False
isCTransfix lc (Manifest t _) = out where
  out
    | null filt = False
    | otherwise = True
  filt = filter (\(tlc, tmt, _) -> tlc == lc && tmt == CTransfix) t

-- Write Inflections to tables
-- Tables organized by lexical category and inflection type
writeInflectionTables :: Language -> InflectionMap -> [Morpheme] -> Text
writeInflectionTables lang inflSys inflMorphs  = "<br>\n" ++ style ++ concat ((\x y -> writeInflectionTableSet lang inflSys inflMorphs x y 1) <$> [Verb .. Pron] <*> [Particle, Prefix, Suffix, Transfix, CTransfix]) where
  style = "<style>table{border-collapse:collapse;}th,td{empty-cells:hide;border:solid 1px black;padding:4px 4px;}th:empty,td:empty,tr:empty{border:0px;padding:0px 0px;}</style>\n"

writeInflectionTableSet :: Language -> InflectionMap -> [Morpheme] -> LexCat -> MorphType -> Int -> Text
writeInflectionTableSet _ _ [] _ _ _ = "No Inflection"
writeInflectionTableSet lang inflSys inflMorphs lc morphType i = out where
  out | cleanInflectionSys inflSys lc morphType i == GramCatExpresses [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] = ""
      | otherwise = "<br>\n" ++ tshow lc ++ " "++ tshow morphType ++ " " ++ tshow i
                    ++ writeInflectionTable lang inflMorphs lc morphType gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols
                    ++ writeInflectionTableSet lang inflSys inflMorphs lc morphType (i+1)
  GramCatExpresses gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols = cleanInflectionSys inflSys lc morphType i


-- Write an inflection system (conjugations/declensions) into an html table
-- Horizontal: Case, Gender, Animacy, Number, Honorific, Transitivity, Evidentiality, Voice, Volition (9)
-- Vertical:   Tense, Aspect, Mood, Person, Clusivity, Definiteness, Specificity, Polarity, Topic     (9)
writeInflectionTable :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Gender] -> [Express Animacy] -> [Express Case] -> [Express Number] -> [Express Definiteness] -> [Express Specificity] -> [Express Topic] -> [Express Person] -> [Express Honorific] -> [Express Polarity] -> [Express Tense] -> [Express Aspect] -> [Express Mood] -> [Express Voice] -> [Express Evidentiality] -> [Express Transitivity] -> [Express Volition] -> Text
writeInflectionTable lang inflMorphs lc morphType gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols  = "<br>\n<table>" ++ title ++ header ++ exarows ++ "\n</table>\n" where

  -- title
  hls = [length cass,length gens,length anis,length nums,length hons,length tras,length evis,length vois,length vols]
  title = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (product hls + 9) ++ "\">" ++ tshow morphType ++ "</th>\n\t</tr>"

  -- header
  horLabels = [map tshow cass, map tshow gens, map tshow anis, map tshow nums, map tshow hons, map tshow tras, map tshow evis, map tshow vois, map tshow vols]
  header = makeHeader horLabels hls

  -- mega rows
  vls = [length tens,length asps,length moos,length pers,1,length defs,length spes,length pols,length tops]

  exarows = concatMap (makeExaRow lang inflMorphs lc morphType vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols) tens

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

makeExaRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Aspect] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Text
makeExaRow lang inflMorphs lc morphType vls asps moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten = exarow where
  exarow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 1 vls)) ++ "\">" ++ tshow ten ++ "</th>" ++ petarows ++ "\n\t</tr>"
  petarows = concatMap (makePetaRow lang inflMorphs lc morphType vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten) asps

makePetaRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Mood] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Text
makePetaRow lang inflMorphs lc morphType vls moos pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp = petarow where
  petarow = "\n\t\t<tr>\n\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 2 vls)) ++ "\">" ++ tshow asp ++ "</th>" ++ terarows ++ "\n\t\t</tr>"
  terarows = concatMap (makeTeraRow lang inflMorphs lc morphType vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp) moos

makeTeraRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Person] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Text
makeTeraRow lang inflMorphs lc morphType vls pers defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo = terarow where
  terarow = "\n\t\t\t<tr>\n\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 3 vls)) ++ "\">" ++ tshow moo ++ "</th>" ++ gigarows ++ "\n\t\t\t</tr>"
  gigarows = concatMap (makeGigaRow lang inflMorphs lc morphType vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo) pers

makeGigaRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Text
makeGigaRow lang inflMorphs lc morphType vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = gigarow where
  gigarow = "\n\t\t\t\t<tr>\n\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 4 vls)) ++ "\">" ++ tshow per ++ "</th>" ++ megarows ++ "\n\t\t\t\t</tr>"
  megarows = makeMegaRow lang inflMorphs lc morphType vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per

makeMegaRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Definiteness] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Text
makeMegaRow lang inflMorphs lc morphType vls defs spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per = megarow where
  megarow = "\n\t\t\t\t\t<tr>\n\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 5 vls)) ++ "\">" ++ "</th>" ++ kilorows ++ "\n\t\t\t\t\t</tr>"
  kilorows = concatMap (makeKiloRow lang inflMorphs lc morphType vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per) defs

makeKiloRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Specificity] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Text
makeKiloRow lang inflMorphs lc morphType vls spes pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def = kilorow where
  kilorow = "\n\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 6 vls)) ++ "\">" ++ tshow def ++ "</th>" ++ hectorows ++ "\n\t\t\t\t\t\t</tr>"
  hectorows = concatMap (makeHectoRow lang inflMorphs lc morphType vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def) spes

makeHectoRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Polarity] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Text
makeHectoRow lang inflMorphs lc morphType vls pols tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe = hectorow where
  hectorow = "\n\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 7 vls)) ++ "\">" ++ tshow spe ++ "</th>" ++ decarows ++ "\n\t\t\t\t\t\t\t</tr>"
  decarows = concatMap (makeDecaRow lang inflMorphs lc morphType vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe) pols

makeDecaRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Topic] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Text
makeDecaRow lang inflMorphs lc morphType vls tops cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol = decarow where
  decarow = "\n\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 8 vls)) ++ "\">" ++ tshow pol ++ "</th>" ++ rows ++ "\n\t\t\t\t\t\t\t\t</tr>"
  rows = concatMap (makeRow lang inflMorphs lc morphType vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol) tops

makeRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Int] -> [Express Case] -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Text
makeRow lang inflMorphs lc morphType vls cass gens anis nums hons tras evis vois vols ten asp moo per def spe pol top = row where
  row = "\n\t\t\t\t\t\t\t\t\t<tr>\n\t\t\t\t\t\t\t\t\t\t<th rowspan=\"" ++ tshow (rowSpanN (drop 9 vls)) ++ "\">" ++ tshow top ++ "</th>" ++ decirows ++ "\n\t\t\t\t\t\t\t\t\t</tr>"
  decirows = concatMap (makeDeciRow lang inflMorphs lc morphType gens anis nums hons tras evis vois vols ten asp moo per def spe pol top) cass

makeDeciRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Gender] -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Text
makeDeciRow lang inflMorphs lc morphType gens anis nums hons tras evis vois vols ten asp moo per def spe pol top cas = concatMap (makeCentiRow lang inflMorphs lc morphType anis nums hons tras evis vois vols ten asp moo per def spe pol top cas) gens

makeCentiRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Animacy] -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Text
makeCentiRow lang inflMorphs lc morphType anis nums hons tras evis vois vols ten asp moo per def spe pol top cas gen = concatMap (makeMilliRow lang inflMorphs lc morphType nums hons tras evis vois vols ten asp moo per def spe pol top cas gen) anis

makeMilliRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Number] -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Text
makeMilliRow lang inflMorphs lc morphType nums hons tras evis vois vols ten asp moo per def spe pol top cas gen ani = concatMap (makeMicroRow lang inflMorphs lc morphType hons tras evis vois vols ten asp moo per def spe pol top cas gen ani) nums

makeMicroRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Honorific] -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Text
makeMicroRow lang inflMorphs lc morphType hons tras evis vois vols ten asp moo per def spe pol top cas gen ani num = concatMap (makeNanoRow lang inflMorphs lc morphType tras evis vois vols ten asp moo per def spe pol top cas gen ani num) hons

makeNanoRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Transitivity] -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Text
makeNanoRow lang inflMorphs lc morphType tras evis vois vols ten asp moo per def spe pol top cas gen ani num hon = concatMap (makePicoRow lang inflMorphs lc morphType evis vois vols ten asp moo per def spe pol top cas gen ani num hon) tras

makePicoRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Evidentiality] -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Text
makePicoRow lang inflMorphs lc morphType evis vois vols ten asp moo per def spe pol top cas gen ani num hon tra = concatMap (makeFemtoRow lang inflMorphs lc morphType vois vols ten asp moo per def spe pol top cas gen ani num hon tra) evis

makeFemtoRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Voice] -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Text
makeFemtoRow lang inflMorphs lc morphType vois vols ten asp moo per def spe pol top cas gen ani num hon tra evi = concatMap (makeAttoRow lang inflMorphs lc morphType vols ten asp moo per def spe pol top cas gen ani num hon tra evi) vois

makeAttoRow :: Language -> [Morpheme] -> LexCat -> MorphType -> [Express Volition] -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Text
makeAttoRow lang inflMorphs lc morphType vols ten asp moo per def spe pol top cas gen ani num hon tra evi voi = cluster where
  cluster = "\n\t\t\t\t\t\t\t\t\t\t<td>" ++ intercalate "</td>\n\t\t\t\t\t\t\t\t\t\t<td>" (map (getMorpheme lang inflMorphs lc morphType ten asp moo per def spe pol top cas gen ani num hon tra evi voi) vols) ++ "</td>"

getMorpheme :: Language -> [Morpheme] -> LexCat -> MorphType -> Express Tense -> Express Aspect -> Express Mood -> Express Person -> Express Definiteness -> Express Specificity -> Express Polarity -> Express Topic -> Express Case -> Express Gender -> Express Animacy -> Express Number -> Express Honorific -> Express Transitivity -> Express Evidentiality -> Express Voice -> Express Volition -> Text
getMorpheme lang inflMorphs lc morphType@Prefix ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
  filt = find (\morph -> getMorphType morph == morphType && getMeaning morph == InflMeaning lc (GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol)) inflMorphs
  output = (++ "–") <$> (writeMorphemeIPA lang <$> filt)
getMorpheme lang inflMorphs lc morphType@Suffix ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
  filt = find (\morph -> getMorphType morph == morphType && getMeaning morph == InflMeaning lc (GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol)) inflMorphs
  output = (++) "–" <$> (writeMorphemeIPA lang <$> filt)
getMorpheme lang inflMorphs lc morphType ten asp moo per def spe pol top cas gen ani num hon tra evi voi vol = fromMaybe "ERROR" output where
  filt = find (\morph -> getMorphType morph == morphType && getLC (getMeaning morph) == lc && getAllExpress (getMeaning morph) == GramCatExpress gen ani cas num def spe top per hon pol ten asp moo voi evi tra vol) inflMorphs
  output = writeMorphemeIPA lang <$> filt
