module Gen.Phonology
( makeDiphInventory
, makeConsonantMap
, makeConsonants
, makeVowelMap
, makeVowels
, makeTones
) where

-- Import
import ClassyPrelude

import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

import Data.Phoneme

import HelperFunctions
import Constants

-- Make diphthongs
makeDiphInventory :: Int -> [Phoneme] -> RVar [Phoneme]
makeDiphInventory n vs = fromMaybe (return []) (join $ safeSample n <$> subseq) where
  subseq = mapM makeDiph $ filter (\(Vowel h1 b1 _ l1, Vowel h2 b2 _ l2) -> (h1 /= h2 || b1 /= b2) && l1 == l2) ((,) <$> vs <*> vs)

makeDiph :: (Phoneme, Phoneme) -> Maybe Phoneme
makeDiph (Vowel h1 b1 r1 _, Vowel h2 b2 r2 _) = Just $ Diphthong h1 b1 r1 h2 b2 r2 NORMAL
makeDiph (_,_) = Nothing

-- Returns a list of places, manners, phonations, and exceptions
makeConsonantMap :: RVar ([Place], [Manner], [Phonation], [Airstream], [Phoneme])
makeConsonantMap = do
  -- Pick places, manners, and phonation
  places <- makePlaces
  manners <- makeManners
  phonations <- makePhonations
  airstreams <- makeAirstream

  let combos = Consonant <$> places <*> manners <*> phonations <*> airstreams

  -- Make exceptions for the place, manner, and airstream dimensions
  rPlaces <- join $ sample <$> uniform 0 (length places - 1) <*> return places
  rManners <- join $ sample <$> uniform 0 (length manners - 1) <*> return manners
  rPhonations <- join $ sample <$> uniform 0 (length phonations - 1) <*> return phonations
  rAirstreams <- join $ sample <$> uniform 0 (length airstreams - 1) <*> return airstreams

  -- Create exceptions
  let exceptions = Consonant <$> rPlaces <*> rManners <*> rPhonations <*> rAirstreams

  -- Add impossible consonants to exceptions
  let exceptions_ = exceptions ++ filter impConsonants combos
  -- This might result in some manners and places being unused...

  return (places, manners, phonations, airstreams, exceptions_)

-- Make the consonant inventory from the consonant map
makeConsonants :: [Place] -> [Manner] -> [Phonation] -> [Airstream] -> [Phoneme] -> [Phoneme]
makeConsonants places manners phonations airstreams exceptions = output where
  -- Create all possible consonants from place, manner, phonation, and phonations
  cns = Consonant <$> places <*> manners <*> phonations <*> airstreams
  -- Filter out exceptions
  output = filter (`notElem` exceptions) cns

-- Make the places of articulation for consonants
makePlaces :: RVar [Place]
makePlaces = do
  n1 <- uniform 0 2
  labial <- sample n1 [BILABIAL, LABIODENTAL]

  n2 <- uniform 1 3
  coronal <- sample n2 [INTERDENTAL, DENTAL, DENTIALVEOLAR, LAMINALALVEOLAR, APICOALVEOLAR, PALATOALVEOLAR, APICALRETROFLEX, RETROFLEX]

  n3 <- uniform 1 3
  dorsal <- sample n3 [ALVEOLOPALATAL, PALATAL, VELAR, UVULAR]

  rad <- choice [[], [PHARYNGEAL]]

  n4 <- uniform 0 2
  laryn <- sample n4 [GLOTTAL, EPIPHARYNGEAL, EPIGLOTTAL]

  let monoPlaces = concat [labial, coronal, dorsal, rad, laryn]

  n5 <- uniform 0 2
  coartPlaces <- replicateM n5 (makeCoart monoPlaces)

  return $ monoPlaces ++ coartPlaces

-- Make a Coarticulated Place
makeCoart :: [Place] -> RVar Place
makeCoart places = do
  n <- uniform 1 (length places - 1)
  let (p1s, p2s) = splitAt n places
  p1 <- choice p1s
  p2 <- choice p2s
  return $ COARTICULATED p1 p2

-- Make the manners of articulation for consonants
makeManners :: RVar [Manner]
makeManners = do
  let stop = [STOP]

  nasal <- choice [[], [NASAL]]

  lflap <- choice [[], [LFLAP]]
  let flap2 = FLAP:lflap
  flap <- choice [[], flap2]

  lat <- choice [[], [LFRICATIVE]]
  sib <- choice [[], [SILIBANT]]
  laff <- choice [[], [LAFFRICATE]]
  saff <- choice [[], [SAFFRICATE]]
  let aff2 = concat [[AFFRICATE], saff, laff]
  aff <- choice [[], aff2]
  let fric2 = concat [[FRICATIVE], aff, sib, lat]
  fric <- choice [[], fric2]

  lapprox <- choice [[], [LAPPROXIMANT]]
  let approx2 = APPROXIMANT:lapprox
  approx <- choice [[], approx2]

  trill <- choice [[], [TRILL]]

  return $ concat [stop, nasal, flap, fric, approx, trill, [CLICK]]

-- Make the phonations (and aspiration) for consonants
makePhonations :: RVar [Phonation]
makePhonations = choice [ [MODAL]
                        , [VOICELESS, MODAL]
                        , [BREATHY, MODAL, CREAKY]
                        , [SLACK, MODAL, STIFF]
                        , [VOICELESS, MODAL, ASPIRATED]
                        , [MODAL, ASPIRATED]
                        ]

makeAirstream :: RVar [Airstream]
makeAirstream = do
  others <- randomSubset [EJECTIVE, IMPLOSIVE, LINGUAL]
  return $ PULMONIC : others

-- Returns a list of heights, backnesses, roundnesss, lengths, tones, and exceptions
makeVowelMap :: RVar ([Height], [Backness], [Roundedness], [Length], [Phoneme])
makeVowelMap = do
  -- pick heights, backnesses, roundnesss, lengths, and tones
  heights <- makeHeights
  backs <- makeBacknesses
  rounds <- makeRoundedneses
  lengths <- makeLengths

  -- make exceptions for the height and backness dimensions
  rHeights <- join $ sample <$> uniform 0 (length heights - 1) <*> return heights
  rBacks <- join $ sample <$> uniform 0 (length backs - 1) <*> return backs

  -- create exceptions
  let exceptions = Vowel <$> rHeights <*> rBacks <*> rounds <*> lengths

  return (heights, backs, rounds, lengths, exceptions)

-- Make the vowel inventory from the vowel map
makeVowels :: [Height] -> [Backness] -> [Roundedness] -> [Length] -> [Phoneme] -> [Phoneme]
makeVowels heights backs rounds lengths exceptions = output where
  -- Create all possible vowels from picked heights, backnesses, roundnesses, lengths, and tones
  vows = Vowel <$> heights <*> backs <*> rounds <*> lengths
  -- Filter out exceptions
  output = filter (`notElem` exceptions) vows

-- Decides how height will be contrasted for vowels
makeHeights :: RVar [Height]
makeHeights = choice [ [MID]
                     , [CLOSE, OPEN]
                     , [CLOSE, MID, OPEN]
                     , [CLOSE, CLOSEMID, OPENMID, OPEN]
                     , [CLOSE, CLOSEMID, MID, OPENMID, OPEN]
                     , [CLOSE, NEARCLOSE, CLOSEMID, OPENMID, NEAROPEN, OPEN]
                     , [CLOSE, NEARCLOSE, CLOSEMID, MID, OPENMID, NEAROPEN, OPEN]
                     ]
-- Decides how backness will be contrasted for vowels
makeBacknesses :: RVar [Backness]
makeBacknesses = choice [ [CENTRAL]
                        , [FRONT, BACK]
                        , [FRONT, CENTRAL, BACK]
                        , [FRONT, NEARFRONT, CENTRAL, NEARBACK, BACK]
                        ]

-- Decides how roundedness will be contrasted for vowels
makeRoundedneses :: RVar [Roundedness]
makeRoundedneses = choice [ [UNROUNDED, ROUNDED]
                          ]

-- Decides how length will be contrasted for vowels
makeLengths :: RVar [Length]
makeLengths = choice [ [NORMAL]
                     , [NORMAL, LONG]
                     , [SHORT, NORMAL]
                     , [SHORT, NORMAL, LONG]
                     ]

-- Tones for syllables
makeTones :: RVar [Tone]
makeTones = choice [ [NONET]
                   , [HIGHT, FALLT, NONET]
                   , [FALLT, PEAKT, NONET]
                   , [LOWT, FALLT, NONET]
                   , [HIGHT, LOWT, NONET]
                   , [HIGHT, MIDT, LOWT]
                   , [HIGHT, MIDT, LOWT, NONET]
                   , [HIGHT, RISET, DIPT, FALLT]
                   , [HIGHT, RISET, DIPT, FALLT, NONET]
                   , [MIDT, LFALLT, HRISET, DIPT]
                   , [MIDT, LFALLT, HRISET, DIPT, NONET]
                   , [MIDT, LOWT, FALLT, HIGHT, RISET]
                   , [MIDT, LOWT, FALLT, HIGHT, RISET, NONET]
                   , [LOWT, MIDT, HIGHT, TOPT, RISET, FALLT]
                   , [LOWT, MIDT, HIGHT, TOPT, RISET, FALLT, NONET]
                   ]
