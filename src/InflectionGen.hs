module InflectionGen
( makeInflectionSystem
, loadInputData
) where

import Prelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Control.Monad
import Data.List

import PhonemeGen
import PhonemeData
import PhonotacticsGen
import OtherData
import InflectionData

-- Input data
data InputData = InputData
    {
      inputGender        :: [[Gender]]
    , inputAnimacy       :: [[Animacy]]
    , inputCase          :: [[Case]]
    , inputNumber        :: [[Number]]
    , inputDefiniteness  :: [[Definiteness]]
    , inputSpecificity   :: [[Specificity]]
    , inputTopic         :: [[Topic]]
    , inputPerson        :: [[Person]]
    , inputClusivity     :: [[Clusivity]]
    , inputHonorific     :: [[Honorific]]
    , inputPolarity      :: [[Polarity]]
    , inputTense         :: [[Tense]]
    , inputAspect        :: [[Aspect]]
    , inputMood          :: [[Mood]]
    , inputVoice         :: [[Voice]]
    , inputEvidentiality :: [[Evidentiality]]
    , inputTransitivity  :: [[Transitivity]]
    , inputVolition      :: [[Volition]]
    }

loadInputData :: IO InputData
loadInputData =
    InputData
        <$> readFeature "raw/grammatical categories/gender.txt"
        <*> readFeature "raw/grammatical categories/animacy.txt"
        <*> readFeature "raw/grammatical categories/case.txt"
        <*> readFeature "raw/grammatical categories/number.txt"
        <*> readFeature "raw/grammatical categories/definiteness.txt"
        <*> readFeature "raw/grammatical categories/specificity.txt"
        <*> readFeature "raw/grammatical categories/topic.txt"
        <*> readFeature "raw/grammatical categories/person.txt"
        <*> readFeature "raw/grammatical categories/clusivity.txt"
        <*> readFeature "raw/grammatical categories/honorific.txt"
        <*> readFeature "raw/grammatical categories/polarity.txt"
        <*> readFeature "raw/grammatical categories/tense.txt"
        <*> readFeature "raw/grammatical categories/aspect.txt"
        <*> readFeature "raw/grammatical categories/mood.txt"
        <*> readFeature "raw/grammatical categories/voice.txt"
        <*> readFeature "raw/grammatical categories/evidentiality.txt"
        <*> readFeature "raw/grammatical categories/transitivity.txt"
        <*> readFeature "raw/grammatical categories/volition.txt"

readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile

-- Create "inflection system"

makeInflectionSystem :: InputData -> RVar (InflectionSystem, [(LexicalCategory, Int, Int, Int, Int)])
makeInflectionSystem idata = do
  (genSys, genNs) <- fooGender idata
  (aniSys, aniNs) <- fooAnimacy idata genNs
  (casSys, casNs) <- fooCase idata aniNs
  (numSys, numNs) <- fooNumber idata casNs
  (defSys, defNs) <- fooDefiniteness idata numNs
  (speSys, speNs) <- fooSpecificity idata defNs
  (topSys, topNs) <- fooTopic idata speNs
  (perSys, perNs) <- fooPerson idata topNs
  (cluSys, cluNs) <- fooClusivity idata perNs
  (honSys, honNs) <- fooHonorific idata cluNs
  (polSys, polNs) <- fooPolarity idata honNs
  (tenSys, tenNs) <- fooTense idata polNs
  (aspSys, aspNs) <- fooAspect idata tenNs
  (mooSys, mooNs) <- fooMood idata aspNs
  (voiSys, voiNs) <- fooVoice idata mooNs
  (eviSys, eviNs) <- fooEvidentiality idata voiNs
  (traSys, traNs) <- fooTransitivity idata eviNs
  (volSys, volNs) <- fooVolition idata traNs
  return (InflectionSystem genSys aniSys casSys numSys defSys speSys topSys perSys cluSys honSys polSys tenSys aspSys mooSys voiSys eviSys traSys volSys, volNs)

bar :: [(LexicalCategory, Int, Int, Int, Int)] -> [(LexicalCategory, ManifestType, Int)] -> [LexicalCategory] -> RVar ([(LexicalCategory, ManifestType, Int)], [(LexicalCategory, Int, Int, Int, Int)])
bar ks ts [] = return (ts,ks)
bar ks ts lcs = do
  (newt, newks) <- rab ks (head lcs)
  bar newks (newt : ts) (tail lcs)

rab :: [(LexicalCategory, Int, Int, Int, Int)] -> LexicalCategory -> RVar ((LexicalCategory, ManifestType, Int), [(LexicalCategory, Int, Int, Int, Int)])
rab lcs lc2 = join out where
    (fu, ba) = partition (\(c, _, _, _, _) -> c == lc2) lcs
    shit
      | null fu = (lc2, 0, 0, 0, 0)
      | otherwise = head fu
    (lc, prep, posp, pref, suff) = shit

    out = choice [ do
                   i <- uniform 1 (prep+1)
                   return ((lc, PreParticle, i), (lc, max i prep, posp, pref, suff) : ba)
                 , do
                   j <- uniform 1 (posp+1)
                   return ((lc, PostParticle, j), (lc, prep, max j posp, pref, suff) : ba)
                 , do
                   k <- uniform 1 (pref+1)
                   return ((lc, Prefix, k), (lc, prep, posp, max k pref, suff) : ba)
                 , do
                   l <- uniform 1 (suff+1)
                   return ((lc, Suffix, l), (lc, prep, posp, pref, max l suff) : ba)
                 ]

fooGender :: InputData -> RVar (Manifest [Gender], [(LexicalCategory, Int, Int, Int, Int)])
fooGender idata = do
  gens <- makeGenders idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ks) <- bar [] [] cats
  choice [(NoManifest, []), (Manifest ts gens, ks)]

fooAnimacy :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Animacy], [(LexicalCategory, Int, Int, Int, Int)])
fooAnimacy idata genNs = do
  anis <- makeAnimacies idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar genNs [] cats
  choice [(NoManifest, genNs), (Manifest ts anis, ns)]

fooCase :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Case], [(LexicalCategory, Int, Int, Int, Int)])
fooCase idata aniNs = do
  cass <- makeCases idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar aniNs [] cats
  choice [(NoManifest, aniNs), (Manifest ts cass, ns)]

fooNumber :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Number], [(LexicalCategory, Int, Int, Int, Int)])
fooNumber idata casNs = do
  nums <- makeNumbers idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar casNs [] cats
  choice [(NoManifest, casNs), (Manifest ts nums, ns)]

fooDefiniteness :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Definiteness], [(LexicalCategory, Int, Int, Int, Int)])
fooDefiniteness idata numNs = do
  defs <- makeDefinitenesses idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar numNs [] cats
  choice [(NoManifest, numNs), (Manifest ts defs, ns)]

fooSpecificity :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Specificity], [(LexicalCategory, Int, Int, Int, Int)])
fooSpecificity idata defNs = do
  spes <- makeSpecificities idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar defNs [] cats
  choice [(NoManifest, defNs), (Manifest ts spes, ns)]

fooTopic :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Topic], [(LexicalCategory, Int, Int, Int, Int)])
fooTopic idata speNs = do
  tops <- makeTopics idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar speNs [] cats
  choice [(NoManifest, speNs), (Manifest ts tops, ns)]

fooPerson :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Person], [(LexicalCategory, Int, Int, Int, Int)])
fooPerson idata topNs = do
  pers <- makePersons idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar topNs [] cats
  choice [(NoManifest, topNs), (Manifest ts pers, ns)]

fooClusivity :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Clusivity], [(LexicalCategory, Int, Int, Int, Int)])
fooClusivity idata perNs = do
  clus <- makeClusivities idata
  i <- uniform 0 3
  cats <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  (ts, ns) <- bar perNs [] cats
  choice [(NoManifest, perNs), (Manifest ts clus, ns)]

fooHonorific :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Honorific], [(LexicalCategory, Int, Int, Int, Int)])
fooHonorific idata cluNs = do
  hons <- makeHonorifics idata
  i <- uniform 0 3
  cats1 <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  j <- uniform 0 4
  cats2 <- (:) Verb <$> sample j [Adv, Prep, Obj, Sub]
  k <- uniform 1 2
  norv <- sample k [cats1, cats2]
  let cats = head norv `union` last norv
  (ts, ns) <- bar cluNs [] cats
  choice [(NoManifest, cluNs), (Manifest ts hons, ns)]

fooPolarity :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Polarity], [(LexicalCategory, Int, Int, Int, Int)])
fooPolarity idata honNs = do
  pols <- makePolarities idata
  i <- uniform 0 3
  cats1 <- (++) [Obj, Sub] <$> sample i [Adj, Prep, Verb]
  j <- uniform 0 4
  cats2 <- (:) Verb <$> sample j [Adv, Prep, Obj, Sub]
  k <- uniform 1 2
  norv <- sample k [cats1, cats2]
  let cats = head norv `union` last norv
  (ts, ns) <- bar honNs [] cats
  choice [(NoManifest, honNs), (Manifest ts pols, ns)]

fooTense :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Tense], [(LexicalCategory, Int, Int, Int, Int)])
fooTense idata polNs = do
  tens <- makeTenses idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar polNs [] cats
  choice [(NoManifest, polNs), (Manifest ts tens, ns)]

fooAspect :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Aspect], [(LexicalCategory, Int, Int, Int, Int)])
fooAspect idata tenNs = do
  asps <- makeAspects idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar tenNs [] cats
  choice [(NoManifest, tenNs), (Manifest ts asps, ns)]

fooMood :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Mood], [(LexicalCategory, Int, Int, Int, Int)])
fooMood idata aspNs = do
  moos <- makeMoods idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar aspNs [] cats
  choice [(NoManifest, aspNs), (Manifest ts moos, ns)]

fooVoice :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Voice], [(LexicalCategory, Int, Int, Int, Int)])
fooVoice idata mooNs = do
  vois <- makeVoices idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar mooNs [] cats
  choice [(NoManifest, mooNs), (Manifest ts vois, ns)]

fooEvidentiality :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Evidentiality], [(LexicalCategory, Int, Int, Int, Int)])
fooEvidentiality idata voiNs = do
  evis <- makeEvidentialities idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar voiNs [] cats
  choice [(NoManifest, voiNs), (Manifest ts evis, ns)]

fooTransitivity :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Transitivity], [(LexicalCategory, Int, Int, Int, Int)])
fooTransitivity idata eviNs = do
  tras <- makeTransitivities idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar eviNs [] cats
  choice [(NoManifest, eviNs), (Manifest ts tras, ns)]

fooVolition :: InputData -> [(LexicalCategory, Int, Int, Int, Int)] -> RVar (Manifest [Volition], [(LexicalCategory, Int, Int, Int, Int)])
fooVolition idata traNs = do
  vols <- makeVolitions idata
  i <- uniform 0 3
  cats <- (:) Verb <$> sample i [Adv, Prep, Obj, Sub]
  (ts, ns) <- bar traNs [] cats
  choice [(NoManifest, traNs), (Manifest ts vols, ns)]

-- Will expand these eventually
makeGenders :: InputData -> RVar [Gender]
makeGenders idata = choice $ inputGender idata

makeAnimacies :: InputData -> RVar [Animacy]
makeAnimacies idata = choice $ inputAnimacy idata

makeCases :: InputData -> RVar [Case]
makeCases idata = choice $ inputCase idata

makeNumbers :: InputData -> RVar [Number]
makeNumbers idata = choice $ inputNumber idata

makeDefinitenesses :: InputData -> RVar [Definiteness]
makeDefinitenesses idata = choice $ inputDefiniteness idata

makeSpecificities ::  InputData -> RVar [Specificity]
makeSpecificities idata = choice $ inputSpecificity idata

makeTopics :: InputData -> RVar [Topic]
makeTopics idata = choice $ inputTopic idata

makePersons :: InputData -> RVar [Person]
makePersons idata = choice $ inputPerson idata

makeClusivities :: InputData -> RVar [Clusivity]
makeClusivities idata = choice $ inputClusivity idata

makeHonorifics :: InputData -> RVar [Honorific]
makeHonorifics idata = choice $ inputHonorific idata

makePolarities :: InputData -> RVar [Polarity]
makePolarities idata = choice $ inputPolarity idata

makeTenses :: InputData -> RVar [Tense]
makeTenses idata = choice $ inputTense idata

makeAspects :: InputData -> RVar [Aspect]
makeAspects idata = choice $ inputAspect idata

makeMoods :: InputData -> RVar [Mood]
makeMoods idata = choice $ inputMood idata

makeVoices :: InputData -> RVar [Voice]
makeVoices idata = choice $ inputVoice idata

makeEvidentialities :: InputData -> RVar [Evidentiality]
makeEvidentialities idata = choice $ inputEvidentiality idata

makeTransitivities :: InputData -> RVar [Transitivity]
makeTransitivities idata = choice $ inputTransitivity idata

makeVolitions :: InputData -> RVar [Volition]
makeVolitions idata = choice $ inputVolition idata
