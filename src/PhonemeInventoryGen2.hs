module PhonemeInventoryGen2
( makeConInventory
, makeVowInventory
, makeDiphInventory
, makeConsonants
, makePlaces
, makeManners
, makePhonations
) where

-- Import
import Prelude
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)
import Data.List
import Control.Monad

import PhonemeType2

-- Load from files
-- Nothing

-- Phoneme data
c = [ [Consonant BILABIAL NASAL VOICELESS "m̥",Consonant BILABIAL NASAL MODAL "m",Consonant LABIODENTAL NASAL VOICELESS "ɱ̊",Consonant LABIODENTAL NASAL MODAL "ɱ",Consonant DENTAL NASAL VOICELESS "n̪̊",Consonant DENTAL NASAL MODAL "n̪",Consonant ALVEOLAR NASAL VOICELESS "n̥",Consonant ALVEOLAR NASAL MODAL "n", Consonant POSTALVEOLAR NASAL VOICELESS "n̠̊",Consonant POSTALVEOLAR NASAL MODAL "n̠",Consonant RETROFLEX NASAL VOICELESS "ɳ̊",Consonant RETROFLEX NASAL MODAL "ɳ",Consonant ALVEOLOPALATAL NASAL VOICELESS "ɲ̟̊",Consonant ALVEOLOPALATAL NASAL MODAL "ɲ̟",Consonant PALATAL NASAL VOICELESS "ɲ̊",Consonant PALATAL NASAL MODAL "ɲ",Consonant VELAR NASAL VOICELESS "ŋ̊",Consonant VELAR NASAL MODAL "ŋ",Consonant UVULAR NASAL VOICELESS "ɴ̥",Consonant UVULAR NASAL MODAL "ɴ",Consonant PHARYNGEAL NASAL VOICELESS [],Consonant PHARYNGEAL NASAL MODAL [],Consonant GLOTTAL NASAL VOICELESS [],Consonant GLOTTAL NASAL MODAL []]
    , [Consonant BILABIAL STOP VOICELESS "p",Consonant BILABIAL STOP MODAL "b",Consonant LABIODENTAL STOP VOICELESS "p̪",Consonant LABIODENTAL STOP MODAL "b̪",Consonant DENTAL STOP VOICELESS "t̪",Consonant DENTAL STOP MODAL "d̪",Consonant ALVEOLAR STOP VOICELESS "t",Consonant ALVEOLAR STOP MODAL "d",Consonant POSTALVEOLAR STOP VOICELESS "t̠",Consonant POSTALVEOLAR STOP MODAL "d̠",Consonant RETROFLEX STOP VOICELESS "ʈ",Consonant RETROFLEX STOP MODAL "ɖ",Consonant ALVEOLOPALATAL STOP VOICELESS "c̟",Consonant ALVEOLOPALATAL STOP MODAL "ɟ̟",Consonant PALATAL STOP VOICELESS "c",Consonant PALATAL STOP MODAL "ɟ",Consonant VELAR STOP VOICELESS "k",Consonant VELAR STOP MODAL "ɡ",Consonant UVULAR STOP VOICELESS "q",Consonant UVULAR STOP MODAL "ɢ",Consonant EPIGLOTTAL STOP VOICELESS "ʡ̥",Consonant EPIGLOTTAL STOP MODAL "ʡ",Consonant GLOTTAL STOP VOICELESS "ʔ",Consonant GLOTTAL STOP MODAL []]
    , [Consonant BILABIAL AFFRICATE VOICELESS "p͡ɸ",Consonant BILABIAL AFFRICATE MODAL "b͡β",Consonant LABIODENTAL AFFRICATE VOICELESS "p̪͡f",Consonant LABIODENTAL AFFRICATE MODAL "b̪͡v",Consonant DENTAL AFFRICATE VOICELESS "t̪͡s̪",Consonant DENTAL AFFRICATE MODAL "d̪͡z̪",Consonant ALVEOLAR AFFRICATE VOICELESS "t͡s",Consonant ALVEOLAR AFFRICATE MODAL "d͡z",Consonant POSTALVEOLAR AFFRICATE VOICELESS "t̠͡ʃ",Consonant POSTALVEOLAR AFFRICATE MODAL "d̠͡ʒ",Consonant RETROFLEX AFFRICATE VOICELESS "ʈ͡ʂ",Consonant RETROFLEX AFFRICATE MODAL "ɖ͡ʐ",Consonant ALVEOLOPALATAL AFFRICATE VOICELESS "c̟͡ɕ",Consonant ALVEOLOPALATAL AFFRICATE MODAL "ɟ̟͡ʑ",Consonant PALATAL AFFRICATE VOICELESS "c͡ç",Consonant PALATAL AFFRICATE MODAL "ɟ͡ʝ",Consonant VELAR AFFRICATE VOICELESS "k͡x",Consonant VELAR AFFRICATE MODAL "ɡ͡ɣ",Consonant UVULAR AFFRICATE VOICELESS "q͡χ",Consonant UVULAR AFFRICATE MODAL "ɢ͡ʁ",Consonant PHARYNGEAL AFFRICATE VOICELESS "ʡ̥͡ħ",Consonant PHARYNGEAL AFFRICATE MODAL "ʡ͡ʕ",Consonant GLOTTAL AFFRICATE VOICELESS "ʔ͡h",Consonant GLOTTAL AFFRICATE MODAL []]
    , [Consonant BILABIAL FRICATIVE VOICELESS "ɸ",Consonant BILABIAL FRICATIVE MODAL "β",Consonant LABIODENTAL FRICATIVE VOICELESS "f",Consonant LABIODENTAL FRICATIVE MODAL "v",Consonant DENTAL FRICATIVE VOICELESS "s̪",Consonant DENTAL FRICATIVE MODAL "z̪",Consonant ALVEOLAR FRICATIVE VOICELESS "s",Consonant ALVEOLAR FRICATIVE MODAL "z",Consonant POSTALVEOLAR FRICATIVE VOICELESS "ʃ",Consonant POSTALVEOLAR FRICATIVE MODAL "ʒ",Consonant RETROFLEX FRICATIVE VOICELESS "ʂ",Consonant RETROFLEX FRICATIVE MODAL "ʐ",Consonant ALVEOLOPALATAL FRICATIVE VOICELESS "ɕ",Consonant ALVEOLOPALATAL FRICATIVE MODAL "ʑ",Consonant PALATAL FRICATIVE VOICELESS "ç",Consonant PALATAL FRICATIVE MODAL "ʝ",Consonant VELAR FRICATIVE VOICELESS "x",Consonant VELAR FRICATIVE MODAL "ɣ",Consonant UVULAR FRICATIVE VOICELESS "χ",Consonant UVULAR FRICATIVE MODAL "ʁ",Consonant PHARYNGEAL FRICATIVE VOICELESS "ħ",Consonant PHARYNGEAL FRICATIVE MODAL "ʕ",Consonant GLOTTAL FRICATIVE VOICELESS "h",Consonant GLOTTAL FRICATIVE MODAL "ɦ"]
    , [Consonant BILABIAL APPROXIMANT VOICELESS "ɸ̝",Consonant BILABIAL APPROXIMANT MODAL "β̞",Consonant LABIODENTAL APPROXIMANT VOICELESS "ʋ̥",Consonant LABIODENTAL APPROXIMANT MODAL "ʋ",Consonant DENTAL APPROXIMANT VOICELESS "ð̞̊ ",Consonant DENTAL APPROXIMANT MODAL "ð̞",Consonant ALVEOLAR APPROXIMANT VOICELESS "ɹ̊",Consonant ALVEOLAR APPROXIMANT MODAL "ɹ",Consonant POSTALVEOLAR APPROXIMANT VOICELESS "ɹ̠̊",Consonant POSTALVEOLAR APPROXIMANT MODAL "ɹ̠",Consonant RETROFLEX APPROXIMANT VOICELESS "ɻ̊",Consonant RETROFLEX APPROXIMANT MODAL "ɻ",Consonant ALVEOLOPALATAL APPROXIMANT VOICELESS "ɻ̠̊",Consonant ALVEOLOPALATAL APPROXIMANT MODAL "ɻ̠",Consonant PALATAL APPROXIMANT VOICELESS "j̊",Consonant PALATAL APPROXIMANT MODAL "j",Consonant VELAR APPROXIMANT VOICELESS "ɰ̊",Consonant VELAR APPROXIMANT MODAL "ɰ",Consonant UVULAR APPROXIMANT VOICELESS "ʁ̞̊",Consonant UVULAR APPROXIMANT MODAL "ʁ̞",Consonant PHARYNGEAL APPROXIMANT VOICELESS "ħ̞",Consonant PHARYNGEAL APPROXIMANT MODAL "ʕ̞",Consonant GLOTTAL APPROXIMANT VOICELESS "h̞",Consonant GLOTTAL APPROXIMANT MODAL "ɦ̞"]
    , [Consonant BILABIAL FLAP VOICELESS "ⱱ̟̊",Consonant BILABIAL FLAP MODAL "ⱱ̟",Consonant LABIODENTAL FLAP VOICELESS "ⱱ̥",Consonant LABIODENTAL FLAP MODAL "ⱱ",Consonant DENTAL FLAP VOICELESS "ɾ̪̊ ",Consonant DENTAL FLAP MODAL "ɾ̪",Consonant ALVEOLAR FLAP VOICELESS "ɾ̥",Consonant ALVEOLAR FLAP MODAL "ɾ",Consonant POSTALVEOLAR FLAP VOICELESS "ɾ̠̊ ",Consonant POSTALVEOLAR FLAP MODAL "ɾ̠",Consonant RETROFLEX FLAP VOICELESS "ɽ̊",Consonant RETROFLEX FLAP MODAL "ɽ",Consonant ALVEOLOPALATAL FLAP VOICELESS "ɽ̠̊",Consonant ALVEOLOPALATAL FLAP MODAL "ɽ̠",Consonant PALATAL FLAP VOICELESS "c̆",Consonant PALATAL FLAP MODAL "ɟ̆",Consonant VELAR FLAP VOICELESS [],Consonant VELAR FLAP MODAL [],Consonant UVULAR FLAP VOICELESS "q̆",Consonant UVULAR FLAP MODAL "ɢ̆",Consonant EPIGLOTTAL FLAP VOICELESS [],Consonant EPIGLOTTAL FLAP MODAL "ʡ̮",Consonant GLOTTAL FLAP VOICELESS [],Consonant GLOTTAL FLAP MODAL []]
    , [Consonant BILABIAL TRILL VOICELESS "ʙ̥",Consonant BILABIAL TRILL MODAL "ʙ",Consonant LABIODENTAL TRILL VOICELESS "ʙ̠̊ ",Consonant LABIODENTAL TRILL MODAL "ʙ̠",Consonant DENTAL TRILL VOICELESS "r̪̊ ",Consonant DENTAL TRILL MODAL "r̪",Consonant ALVEOLAR TRILL VOICELESS "r̥",Consonant ALVEOLAR TRILL MODAL "r",Consonant POSTALVEOLAR TRILL VOICELESS "r̠̊",Consonant POSTALVEOLAR TRILL MODAL "r̠",Consonant RETROFLEX TRILL VOICELESS "ɽ͡r̥",Consonant RETROFLEX TRILL MODAL "ɽ͡r",Consonant ALVEOLOPALATAL TRILL VOICELESS "[]",Consonant ALVEOLOPALATAL TRILL MODAL "[]",Consonant PALATAL TRILL VOICELESS "[]",Consonant PALATAL TRILL MODAL "[]",Consonant VELAR TRILL VOICELESS [],Consonant VELAR TRILL MODAL [],Consonant UVULAR TRILL VOICELESS "ʀ̥",Consonant UVULAR TRILL MODAL "ʀ",Consonant EPIGLOTTAL TRILL VOICELESS "ʜ",Consonant EPIGLOTTAL TRILL MODAL "ʢ",Consonant GLOTTAL TRILL VOICELESS [],Consonant GLOTTAL TRILL MODAL []]
    , [Consonant BILABIAL LAFFRICATE VOICELESS [],Consonant BILABIAL LAFFRICATE MODAL [],Consonant LABIODENTAL LAFFRICATE VOICELESS [],Consonant LABIODENTAL LAFFRICATE MODAL [],Consonant DENTAL LAFFRICATE VOICELESS "t̪͡ɬ̪̊",Consonant DENTAL LAFFRICATE MODAL "d̪͡ɮ̪",Consonant ALVEOLAR LAFFRICATE VOICELESS "t͡ɬ",Consonant ALVEOLAR LAFFRICATE MODAL "d͡ɮ",Consonant POSTALVEOLAR LAFFRICATE VOICELESS "",Consonant POSTALVEOLAR LAFFRICATE MODAL "d̠͡ɮ̠",Consonant RETROFLEX LAFFRICATE VOICELESS "ʈ͡ɭ̥˔",Consonant RETROFLEX LAFFRICATE MODAL "ɖ͡ɭ˔",Consonant ALVEOLOPALATAL LAFFRICATE VOICELESS "c̟͡ʎ̟̝̊",Consonant ALVEOLOPALATAL LAFFRICATE MODAL "ɟ̟͡ʎ̟̝",Consonant PALATAL LAFFRICATE VOICELESS "c͡ʎ̝̊",Consonant PALATAL LAFFRICATE MODAL "ɟ͡ʎ̝̊",Consonant VELAR LAFFRICATE VOICELESS "k͡ʟ̝̊",Consonant VELAR LAFFRICATE MODAL "ɡ͡ʟ̝",Consonant UVULAR LAFFRICATE VOICELESS "q͡ʟ̝̠̊",Consonant UVULAR LAFFRICATE MODAL "ɢ͡ʟ̝̠",Consonant PHARYNGEAL LAFFRICATE VOICELESS [],Consonant PHARYNGEAL LAFFRICATE MODAL [],Consonant GLOTTAL LAFFRICATE VOICELESS [],Consonant GLOTTAL LAFFRICATE MODAL []]
    , [Consonant BILABIAL LFRICATIVE VOICELESS [],Consonant BILABIAL LFRICATIVE MODAL [],Consonant LABIODENTAL LFRICATIVE VOICELESS [],Consonant LABIODENTAL LFRICATIVE MODAL [],Consonant DENTAL LFRICATIVE VOICELESS "ɬ̪̊",Consonant DENTAL LFRICATIVE MODAL "ɮ̪",Consonant ALVEOLAR LFRICATIVE VOICELESS "ɬ",Consonant ALVEOLAR LFRICATIVE MODAL "ɮ",Consonant POSTALVEOLAR LFRICATIVE VOICELESS "ɬ̠",Consonant POSTALVEOLAR LFRICATIVE MODAL "ɮ̠",Consonant RETROFLEX LFRICATIVE VOICELESS "ɭ̥˔",Consonant RETROFLEX LFRICATIVE MODAL "ɭ˔",Consonant ALVEOLOPALATAL LFRICATIVE VOICELESS "ʎ̟̝̊",Consonant ALVEOLOPALATAL LFRICATIVE MODAL "ʎ̟̝",Consonant PALATAL LFRICATIVE VOICELESS "ʎ̝̊",Consonant PALATAL LFRICATIVE MODAL "ʎ̝̊",Consonant VELAR LFRICATIVE VOICELESS "ʟ̝̊",Consonant VELAR LFRICATIVE MODAL "ʟ̝",Consonant UVULAR LFRICATIVE VOICELESS "ʟ̝̠̊",Consonant UVULAR LFRICATIVE MODAL "ʟ̝̠",Consonant PHARYNGEAL LFRICATIVE VOICELESS [],Consonant PHARYNGEAL LFRICATIVE MODAL [],Consonant GLOTTAL LFRICATIVE VOICELESS [],Consonant GLOTTAL LFRICATIVE MODAL []]
    , [Consonant BILABIAL LAPPROXIMANT VOICELESS [],Consonant BILABIAL LAPPROXIMANT MODAL [],Consonant LABIODENTAL LAPPROXIMANT VOICELESS [],Consonant LABIODENTAL LAPPROXIMANT MODAL [],Consonant DENTAL LAPPROXIMANT VOICELESS "l̪̊",Consonant DENTAL LAPPROXIMANT MODAL "l̪",Consonant ALVEOLAR LAPPROXIMANT VOICELESS "l̥",Consonant ALVEOLAR LAPPROXIMANT MODAL "l",Consonant POSTALVEOLAR LAPPROXIMANT VOICELESS "l̠̊",Consonant POSTALVEOLAR LAPPROXIMANT MODAL "l̠",Consonant RETROFLEX LAPPROXIMANT VOICELESS "ɭ̥",Consonant RETROFLEX LAPPROXIMANT MODAL "ɭ",Consonant ALVEOLOPALATAL LAPPROXIMANT VOICELESS "ʎ̟̊ ",Consonant ALVEOLOPALATAL LAPPROXIMANT MODAL "ʎ̟",Consonant PALATAL LAPPROXIMANT VOICELESS "ʎ̥",Consonant PALATAL LAPPROXIMANT MODAL "ʎ",Consonant VELAR LAPPROXIMANT VOICELESS "ʟ̥",Consonant VELAR LAPPROXIMANT MODAL "ʟ",Consonant UVULAR LAPPROXIMANT VOICELESS "ʟ̠̊",Consonant UVULAR LAPPROXIMANT MODAL "ʟ̠",Consonant PHARYNGEAL LAPPROXIMANT VOICELESS [],Consonant PHARYNGEAL LAPPROXIMANT MODAL [],Consonant GLOTTAL LAPPROXIMANT VOICELESS [],Consonant GLOTTAL LAPPROXIMANT MODAL []]
    , [Consonant BILABIAL LFLAP VOICELESS [],Consonant BILABIAL LFLAP MODAL [],Consonant LABIODENTAL LFLAP VOICELESS [],Consonant LABIODENTAL LFLAP MODAL [],Consonant DENTAL LFLAP VOICELESS "ɺ̪̊",Consonant DENTAL LFLAP MODAL "ɺ̪",Consonant ALVEOLAR LFLAP VOICELESS "ɺ̥",Consonant ALVEOLAR LFLAP MODAL "ɺ",Consonant POSTALVEOLAR LFLAP VOICELESS "ɺ̠̊",Consonant POSTALVEOLAR LFLAP MODAL "ɺ̠",Consonant RETROFLEX LFLAP VOICELESS "ɺ̢̊",Consonant RETROFLEX LFLAP MODAL "ɺ̢",Consonant ALVEOLOPALATAL LFLAP VOICELESS "ʎ̯̟̊",Consonant ALVEOLOPALATAL LFLAP MODAL "ʎ̯̟",Consonant PALATAL LFLAP VOICELESS "ʎ̯̊",Consonant PALATAL LFLAP MODAL "ʎ̯",Consonant VELAR LFLAP VOICELESS "ʟ̥̆",Consonant VELAR LFLAP MODAL "ʟ̆",Consonant UVULAR LFLAP VOICELESS "ʟ̠̆̊",Consonant UVULAR LFLAP MODAL "ʟ̠̆",Consonant PHARYNGEAL LFLAP VOICELESS [],Consonant PHARYNGEAL LFLAP MODAL [],Consonant GLOTTAL LFLAP VOICELESS [],Consonant GLOTTAL LFLAP MODAL []]
    ]

v = [ [Vowel CLOSE BACK UNROUNDED "i",Vowel CLOSE BACK UNROUNDED "y",Vowel CLOSE NEARBACK UNROUNDED "ɪ̝",Vowel CLOSE NEARBACK UNROUNDED "ʏ̝",Vowel CLOSE CENTRAL UNROUNDED "ɨ",Vowel CLOSE CENTRAL UNROUNDED "ʉ",Vowel CLOSE NEARFRONT UNROUNDED "ɯ̟",Vowel CLOSE NEARFRONT UNROUNDED "ʊ̝",Vowel CLOSE FRONT UNROUNDED "ɯ",Vowel CLOSE FRONT UNROUNDED "u"]
    , [Vowel NEARCLOSE BACK UNROUNDED "ɪ̟",Vowel NEARCLOSE BACK UNROUNDED "ʏ̟",Vowel NEARCLOSE NEARBACK UNROUNDED "ɪ",Vowel NEARCLOSE NEARBACK UNROUNDED "ʏ",Vowel NEARCLOSE CENTRAL UNROUNDED "ɪ̈",Vowel NEARCLOSE CENTRAL UNROUNDED "ʊ̈",Vowel NEARCLOSE NEARFRONT UNROUNDED "ɯ̽",Vowel NEARCLOSE NEARFRONT UNROUNDED "ʊ",Vowel NEARCLOSE FRONT UNROUNDED "ɯ̞",Vowel NEARCLOSE FRONT UNROUNDED "ʊ̠"]
    , [Vowel CLOSEMID BACK UNROUNDED "e",Vowel CLOSEMID BACK UNROUNDED "ø",Vowel CLOSEMID NEARBACK UNROUNDED "ë",Vowel CLOSEMID NEARBACK UNROUNDED "ø̈",Vowel CLOSEMID CENTRAL UNROUNDED "ɘ",Vowel CLOSEMID CENTRAL UNROUNDED "ɵ",Vowel CLOSEMID NEARFRONT UNROUNDED "ɤ̈",Vowel CLOSEMID NEARFRONT UNROUNDED "ö",Vowel CLOSEMID FRONT UNROUNDED "ɤ",Vowel CLOSEMID FRONT UNROUNDED "o"]
    , [Vowel MID BACK UNROUNDED "e̞",Vowel MID BACK UNROUNDED "ø̞",Vowel MID NEARBACK UNROUNDED "ë̞",Vowel MID NEARBACK UNROUNDED "ø̞̈",Vowel MID CENTRAL UNROUNDED "ə",Vowel MID CENTRAL UNROUNDED "ɵ̞",Vowel MID NEARFRONT UNROUNDED "ɤ̞̈",Vowel MID NEARFRONT UNROUNDED "ö̞",Vowel MID FRONT UNROUNDED "ɤ̞",Vowel MID FRONT UNROUNDED "o̞"]
    , [Vowel OPENMID BACK UNROUNDED "ɛ",Vowel OPENMID BACK UNROUNDED "œ",Vowel OPENMID NEARBACK UNROUNDED "ɛ̈",Vowel OPENMID NEARBACK UNROUNDED "œ̈",Vowel OPENMID CENTRAL UNROUNDED "ɜ",Vowel OPENMID CENTRAL UNROUNDED "ɞ",Vowel OPENMID NEARFRONT UNROUNDED "ʌ̈",Vowel OPENMID NEARFRONT UNROUNDED "ɔ̈",Vowel OPENMID FRONT UNROUNDED "ʌ",Vowel OPENMID FRONT UNROUNDED "ɔ"]
    , [Vowel NEAROPEN BACK UNROUNDED "æ",Vowel NEAROPEN BACK UNROUNDED "œ̞",Vowel NEAROPEN NEARBACK UNROUNDED "a̽",Vowel NEAROPEN NEARBACK UNROUNDED "ɶ̽",Vowel NEAROPEN CENTRAL UNROUNDED "ɐ",Vowel NEAROPEN CENTRAL UNROUNDED "ɞ̞",Vowel NEAROPEN NEARFRONT UNROUNDED "ɑ̽",Vowel NEAROPEN NEARFRONT UNROUNDED "ɒ̽",Vowel NEAROPEN FRONT UNROUNDED "ʌ̞",Vowel NEAROPEN FRONT UNROUNDED "ɔ̞"]
    , [Vowel OPEN BACK UNROUNDED "a",Vowel OPEN BACK UNROUNDED "ɶ",Vowel OPEN NEARBACK UNROUNDED "a̠",Vowel OPEN NEARBACK UNROUNDED "ɶ̠",Vowel OPEN CENTRAL UNROUNDED "ä",Vowel OPEN CENTRAL UNROUNDED "ɒ̈",Vowel OPEN NEARFRONT UNROUNDED "ɑ̟",Vowel OPEN NEARFRONT UNROUNDED "ɒ̟",Vowel OPEN FRONT UNROUNDED "ɑ",Vowel OPEN FRONT UNROUNDED "ɒ"]
    ]

-- Naive: Pick purely random inventories
makeConInventory :: Int -> RVar [Phoneme]
makeConInventory n =  sample n $ filter (not . null . csymbol) $ concat c

makeVowInventory :: Int -> RVar [Phoneme]
makeVowInventory n = sample n $ filter (not . null . vsymbol) $ concat v

makeDiphInventory :: Int -> [Phoneme] -> RVar [Phoneme]
makeDiphInventory n vs = sample n subseq where
  subseq = map makeDiph $ filter (uncurry (/=)) ((,) <$> vs <*> vs)

makeDiph :: (Phoneme, Phoneme) -> Phoneme
makeDiph (Vowel h1 b1 r1 s1, Vowel h2 b2 r2 s2) = Diphthong h1 b1 r1 s1 h2 b2 r2 s2

-- Minimum: Picks rows and contrasts and stuff

makePlaces :: RVar [Place]
makePlaces = concat <$> sequence [ join $ choice [return [], return [LABIAL], return [BILABIAL, LABIODENTAL]]
                                 , join $ choice [return [CORONAL], concat <$> sequence [choice [[DENTIALVEOLAR], [DENTAL, ALVEOLAR, POSTALVEOLAR]], return [RETROFLEX]]]
                                 , join $ choice [return [DORSAL], concat <$> join (sample <$> uniform 2 3 <*> sequence [choice [[PALATAL], [ALVEOLOPALATAL, PALATAL]], return [VELAR], return[UVULAR]])]
                                 , join $ choice [return [], return [LARYNGEAL], concat <$> sequence [choice [[EPIPHARYNGEAL], [PHARYNGEAL, EPIGLOTTAL]], return [GLOTTAL]]]
                                 ]

makeManners :: RVar [Manner]
makeManners = concat <$> sequence [ return [STOP]
                                  , choice [[], [NASAL]]
                                  , join $ choice [return [], concat <$> sequence [return [FLAP], choice [[], [LFLAP]]]]
                                  , join $ choice [return [], concat <$> sequence [return [FRICATIVE], join $ choice [return [], concat <$> sequence [return [AFFRICATE], choice [[], [SAFFRICATE]], choice [[], [LAFFRICATE]]]], choice [[], [SILIBANT]], choice [[], [LFRICATIVE]]]]
                                  , join $ choice [return [], concat <$> sequence [return [APPROXIMANT], choice [[], [LAPPROXIMANT]]]]
                                  , choice [[], [TRILL]]
                                  ]

makePhonations :: RVar [Phonation]
makePhonations = choice [[MODAL], [VOICELESS, MODAL], [BREATHY, MODAL, CREAKY], [SLACK, MODAL, STIFF], [VOICELESS, MODAL, ASPIRATED], [MODAL, ASPIRATED]]

makeConsonants :: [Place] -> [Manner] -> [Phonation] -> RVar [Phoneme]
makeConsonants places manners phonations = output where
  -- Create all possible consonants from picked place, manner, and phonation
  cons = Consonant <$> places <*> manners <*> phonations <*> [""]
  -- Filter out impossible combinations
  filt = filter (\(Consonant p m h _) -> impConsonants p m h) cons
  -- Add holes...
  rPlaces = join $ sample <$> uniform 0 (length places - 1) <*> return places
  rManners = join $ sample <$> uniform 0 (length manners - 1) <*> return manners
  holed = do
    rp <- rPlaces
    rm <- rManners
    let combos = (,) <$> rp <*> rm
    return $ filter (\(Consonant p m h _) -> (p,m) `notElem` combos) filt
  -- Assign IPA symbol
  output = map assignIPA <$> holed

-- Assigns IPA symbol to consonant
assignIPA :: Phoneme -> Phoneme
assignIPA (Consonant p m ASPIRATED s) = Consonant p m ASPIRATED (retrieveSymbol (Consonant p m MODAL s) ++ "\688")
assignIPA (Consonant p m CREAKY s) = Consonant p m CREAKY (retrieveSymbol (Consonant p m MODAL s) ++ "\816")
assignIPA (Consonant p m STIFF s) = Consonant p m STIFF (retrieveSymbol (Consonant p m MODAL s) ++ "\812")
assignIPA (Consonant p m SLACK s) = Consonant p m SLACK (retrieveSymbol (Consonant p m MODAL s) ++ "\805")
assignIPA (Consonant p m BREATHY s) = Consonant p m BREATHY (retrieveSymbol (Consonant p m MODAL s) ++ "\804")
assignIPA (Consonant p m h s) = Consonant p m h (retrieveSymbol (Consonant p m h s))

retrieveSymbol :: Phoneme -> String
retrieveSymbol (Consonant p m h _)
    | not.null $ searchIPA p m h = csymbol $ head $ searchIPA p m h
    | (p == LABIAL) && (not . null $ searchIPA BILABIAL m h) = csymbol $ head $ searchIPA BILABIAL m h
    | ((p == CORONAL) || (p == DENTIALVEOLAR)) && (not . null $ searchIPA ALVEOLAR m h) = csymbol $ head $ searchIPA ALVEOLAR m h
    | (p == DORSAL) && (not . null $ searchIPA VELAR m h) = csymbol $ head $ searchIPA VELAR m h
    | (p == DORSAL) && (not . null $ searchIPA UVULAR m h) = csymbol $ head $ searchIPA UVULAR m h
    | (p == LARYNGEAL) && (not . null $ searchIPA GLOTTAL m h) = csymbol $ head $ searchIPA GLOTTAL m h
    | (p == LARYNGEAL) && (not . null $ searchIPA EPIGLOTTAL m h) = csymbol $ head $ searchIPA EPIGLOTTAL m h
    | (p == LARYNGEAL) && (not . null $ searchIPA PHARYNGEAL m h) = csymbol $ head $ searchIPA PHARYNGEAL m h
    | (p == EPIPHARYNGEAL) && (not . null $ searchIPA EPIGLOTTAL m h) = csymbol $ head $ searchIPA EPIGLOTTAL m h
    | (p == EPIPHARYNGEAL) && (not . null $ searchIPA PHARYNGEAL m h) = csymbol $ head $ searchIPA PHARYNGEAL m h
    | (p == EPIGLOTTAL) && (not . null $ searchIPA PHARYNGEAL m h) = csymbol (head $ searchIPA PHARYNGEAL m h) ++ "\799"
    | (p == PHARYNGEAL) && (not . null $ searchIPA EPIGLOTTAL m h) = csymbol (head $ searchIPA EPIGLOTTAL m h) ++ "\800"
    | otherwise = "ERROR"

searchIPA :: Place -> Manner -> Phonation -> [Phoneme]
searchIPA p m h = filt where
  cons = filter (not . null . csymbol) $ concat c
  filt = filter (\(Consonant pc mc hc _) -> p == pc && m == mc && h == hc) cons

-- Impossible consonants filter
impConsonants :: Place -> Manner -> Phonation -> Bool
impConsonants p m h
  | p `notElem` [CORONAL, DENTIALVEOLAR, DENTAL, ALVEOLAR, POSTALVEOLAR, RETROFLEX] && m `elem` [SILIBANT, SAFFRICATE] = False
  | p `elem` [LABIAL, BILABIAL, LABIODENTAL, LARYNGEAL, EPIPHARYNGEAL, PHARYNGEAL, EPIGLOTTAL, GLOTTAL] && m `elem` [LAFFRICATE, LAPPROXIMANT, LFRICATIVE, LFRICATIVE, LFLAP] = False
  | p `elem` [DORSAL, ALVEOLOPALATAL, PALATAL, VELAR, UVULAR, GLOTTAL] && m `elem` [FLAP, TRILL] = False
  | p == GLOTTAL && m `elem` [STOP, AFFRICATE] && h /= VOICELESS = False
  | m /= STOP && h == ASPIRATED = False
  | p `elem` [LARYNGEAL, EPIPHARYNGEAL, PHARYNGEAL, EPIGLOTTAL, GLOTTAL] && m == NASAL = False
  | otherwise = True
