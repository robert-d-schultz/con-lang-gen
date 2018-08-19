module Gen.Phonology
( makeDiphInventory
, makeConsonantMap
, makeConsonants
, makeVowelMap
, makeVowels
) where

-- Import
import Prelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.List
import Control.Monad

import Data.Phoneme
import Constants

-- Make diphthongs
makeDiphInventory :: Int -> [Phoneme] -> RVar [Phoneme]
makeDiphInventory n vs = sample n subseq where
  subseq = map makeDiph $ filter (\(Vowel h1 b1 r1 l1 t1, Vowel h2 b2 r2 l2 t2) -> (h1 /= h2 || b1 /= b2) && l1 == l2) ((,) <$> vs <*> vs)

makeDiph :: (Phoneme, Phoneme) -> Phoneme
makeDiph (Vowel h1 b1 r1 l1 t1, Vowel h2 b2 r2 l2 t2) = Diphthong h1 b1 r1 h2 b2 r2 NORMAL t1

-- Returns a list of places, manners, phonations, and exceptions
makeConsonantMap :: RVar ([Place], [Manner], [Phonation], [Phoneme])
makeConsonantMap = do
  -- pick places, manners, and phonation
  places <- makePlaces
  manners <- makeManners
  phonations <- makePhonations

  let combos = Consonant <$> places <*> manners <*> phonations

  -- make exceptions for the place and manner dimensions
  rPlaces <- join $ sample <$> uniform 0 (length places - 1) <*> return places
  rManners <- join $ sample <$> uniform 0 (length manners - 1) <*> return manners

  -- create exceptions
  let exceptions = Consonant <$> rPlaces <*> rManners <*> phonations

  -- add impossible consonants to exceptions
  let exceptions_ = exceptions ++ filter impConsonants combos
  -- this might result in some manners and places being unused...

  return (places, manners, phonations, exceptions_)

-- Make the consonant inventory from the consonant map
makeConsonants :: [Place] -> [Manner] -> [Phonation] -> [Phoneme] -> [Phoneme]
makeConsonants places manners phonations exceptions = output where
  -- Create all possible consonants from place, manner, and phonation
  cons = Consonant <$> places <*> manners <*> phonations
  -- Filter out exceptions
  output = filter (`notElem` exceptions) cons

-- Make the places of articulation for consonants
makePlaces :: RVar [Place]
makePlaces = do
  labial <- choice [[], [LABIAL], [BILABIAL, LABIODENTAL]]

  dental <- choice [[DENTIALVEOLAR], [DENTAL, ALVEOLAR, POSTALVEOLAR]]
  let coronal2 = concat [dental, [RETROFLEX]]
  coronal <- choice [[CORONAL], coronal2]

  apalatal <- choice [[], [ALVEOLOPALATAL]]
  let palatal = concat [[PALATAL], apalatal]
  n <- uniform 2 3
  dorsal2 <- sample n [palatal, [VELAR], [UVULAR]]
  dorsal <- choice [[DORSAL], concat dorsal2]

  epiph <- choice [[EPIPHARYNGEAL], [PHARYNGEAL, EPIGLOTTAL]]
  let laryn2 = concat [epiph, [GLOTTAL]]
  laryn <- choice [[], [LARYNGEAL], laryn2]

  return $ concat [labial, coronal, dorsal, laryn]

-- Make the manners of articulation for consonants
makeManners :: RVar [Manner]
makeManners = do
  let stop = [STOP]

  nasal <- choice [[], [NASAL]]

  lflap <- choice [[], [LFLAP]]
  let flap2 = concat [[FLAP], lflap]
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
  let approx2 = concat [[APPROXIMANT], lapprox]
  approx <- choice [[], approx2]

  trill <- choice [[], [TRILL]]

  return $ concat [stop, nasal, flap, fric, approx, trill]

-- Make the phonations (and aspiration) for consonants
makePhonations :: RVar [Phonation]
makePhonations = choice [ [MODAL]
                        , [VOICELESS, MODAL]
                        , [BREATHY, MODAL, CREAKY]
                        , [SLACK, MODAL, STIFF]
                        , [VOICELESS, MODAL, ASPIRATED]
                        , [MODAL, ASPIRATED]
                        ]

-- Returns a list of heights, backnesses, roundnesss, lengths, tones, and exceptions
makeVowelMap :: RVar ([Height], [Backness], [Roundedness], [Length], [Tone], [Phoneme])
makeVowelMap = do
  -- pick heights, backnesses, roundnesss, lengths, and tones
  heights <- makeHeights
  backs <- makeBacknesses
  rounds <- makeRoundedneses
  lengths <- makeLengths
  tones <- makeTones

  -- make exceptions for the height and backness dimensions
  rHeights <- join $ sample <$> uniform 0 (length heights - 1) <*> return heights
  rBacks <- join $ sample <$> uniform 0 (length backs - 1) <*> return backs

  -- create exceptions
  let exceptions = Vowel <$> rHeights <*> rBacks <*> rounds <*> lengths <*> tones

  return (heights, backs, rounds, lengths, tones, exceptions)

-- Make the vowel inventory from the vowel map
makeVowels :: [Height] -> [Backness] -> [Roundedness] -> [Length] -> [Tone] -> [Phoneme] -> [Phoneme]
makeVowels heights backs rounds lengths tones exceptions = output where
  -- Create all possible vowels from picked heights, backnesses, roundnesses, lengths, and tones
  vows = Vowel <$> heights <*> backs <*> rounds <*> lengths <*> tones
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

-- Decides how tone will be contrasted for vowels
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
