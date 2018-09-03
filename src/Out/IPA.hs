module Out.IPA
( writePhonemeIPA
, writeToneLetterIPA
, writeStressMarkIPA
) where

import ClassyPrelude

import Data.Phoneme

-- parse phoneme into a string
writePhonemeIPA :: Phoneme -> Text
writePhonemeIPA Blank = ""
writePhonemeIPA (Consonant p m h a) = retrieveCSymbol p m h a
writePhonemeIPA (Vowel h b r l) = retrieveVSymbol h b r l
writePhonemeIPA (Diphthong h1 b1 r1 h2 b2 r2 l) = retrieveVSymbol h1 b1 r1 l ++ retrieveVSymbol h2 b2 r2 l ++ "\815"

-- Decides what IPA symbol to use, essentially
retrieveCSymbol :: Place -> Manner -> Phonation -> Airstream -> Text
retrieveCSymbol p m h a
    | isJust $ searchCIPA p m h a = fromMaybe "ERROR" (searchCIPA p m h a)
    -- Aspiration
    | h == ASPIRATED = retrieveCSymbol p m MODAL a ++ "ʰ"
    -- Ejective
    | a == EJECTIVE = retrieveCSymbol p m h PULMONIC ++ "ʼ"
    -- Cooarticulated
    | (\case COARTICULATED{} -> True; _ -> False) p = retrieveCSymbol (getPlaceA p) m h a ++ "\865" ++ retrieveCSymbol (getPlaceB p) m h a
    -- Affricates are Stop+("\865")+Fricative
    | m == SAFFRICATE = retrieveCSymbol p STOP h a ++ "\865" ++ retrieveCSymbol p SILIBANT h a
    | m == AFFRICATE = retrieveCSymbol p STOP h a ++ "\865" ++ retrieveCSymbol p FRICATIVE h a
    | m == LAFFRICATE = retrieveCSymbol p STOP h a ++ "\865" ++ retrieveCSymbol p LFRICATIVE h a
    -- Phonation
    | h == CREAKY = retrieveCSymbol p m MODAL a ++ "\816"
    | h == STIFF = retrieveCSymbol p m MODAL a ++ "\812"
    | h == SLACK = retrieveCSymbol p m MODAL a ++ "\805"
    | h == BREATHY = retrieveCSymbol p m MODAL a ++ "\804"
    | h == VOICELESS = retrieveCSymbol p m MODAL a ++ "\805"
    | h == MODAL && isJust (searchCIPA p m VOICELESS a) = retrieveCSymbol p m VOICELESS a ++ "\812"
    -- Place of articulation
    | p == LABIODENTAL = retrieveCSymbol BILABIAL m h a ++ "\810"
    | p == LINGUOLABIAL && isJust (searchCIPA DENTAL m h a) = retrieveCSymbol DENTAL m h a ++ "\828"
    | p == LINGUOLABIAL = retrieveCSymbol ALVEOLAR m h a ++ "\828"
    | p == INTERDENTAL && isJust (searchCIPA DENTAL m h a) = retrieveCSymbol DENTAL m h a ++ "\810" ++ "\838"
    | p == INTERDENTAL = retrieveCSymbol ALVEOLAR m h a ++ "\810" ++ "\838"
    | p == DENTAL = retrieveCSymbol ALVEOLAR m h a ++ "\810"
    | p == DENTIALVEOLAR = retrieveCSymbol ALVEOLAR m h a ++ "\810" ++ "\827"
    | p == LAMINALALVEOLAR = retrieveCSymbol ALVEOLAR m h a ++ "\827"
    | p == APICOALVEOLAR = retrieveCSymbol ALVEOLAR m h a ++ "\826"
    | p == PALATOALVEOLAR && isJust (searchCIPA POSTALVEOLAR m h a) = retrieveCSymbol POSTALVEOLAR m h a ++ "\827"
    | p == PALATOALVEOLAR = retrieveCSymbol ALVEOLAR m h a ++ "\800"
    | p == APICALRETROFLEX = retrieveCSymbol ALVEOLAR m h a ++ "\803"
    | p == ALVEOLOPALATAL && isJust (searchCIPA POSTALVEOLAR m h a) = retrieveCSymbol POSTALVEOLAR m h a ++ "\800"
    | p == ALVEOLOPALATAL = retrieveCSymbol PALATAL m h a ++ "\799"
    | p == ALVEOLAR = retrieveCSymbol DENTAL m h a ++ "\839"
    | p == POSTALVEOLAR = retrieveCSymbol ALVEOLAR m h a ++ "\800"
    -- Place advancing ("\799") and retracting ("\800")
    | (p == EPIPHARYNGEAL) && isJust (searchCIPA EPIGLOTTAL m h a) = retrieveCSymbol EPIGLOTTAL m h a ++ "\800"
    | (p == EPIPHARYNGEAL) && isJust (searchCIPA PHARYNGEAL m h a) = retrieveCSymbol PHARYNGEAL m h a ++ "\799"
    | p == EPIGLOTTAL = retrieveCSymbol PHARYNGEAL m h a ++ "\800"
    | (p == PHARYNGEAL) && isJust (searchCIPA EPIGLOTTAL m h a) = retrieveCSymbol EPIGLOTTAL m h a ++ "\799"
    | (p == UVULAR) && isJust (searchCIPA VELAR m h a) = retrieveCSymbol VELAR m h a ++ "\800"
    -- Manner raising ("\797") and lowering ("\798")
    | m == FRICATIVE = retrieveCSymbol p APPROXIMANT h a ++ "\797"
    | m == APPROXIMANT && isJust (searchCIPA p FRICATIVE h a) = retrieveCSymbol p FRICATIVE h a ++ "\798"
    | m == LFRICATIVE = retrieveCSymbol p LAPPROXIMANT h a ++ "\797"
    | m == LAPPROXIMANT && isJust (searchCIPA p LFRICATIVE h a) = retrieveCSymbol p LFRICATIVE h a ++ "\798"
    -- Flaps are Short("\774") stops or approximants
    | m == FLAP = retrieveCSymbol p STOP h a ++ "\774"
    | m == LFLAP = retrieveCSymbol p LAPPROXIMANT h a ++ "\774"
    -- Otherwise error
    | otherwise = "ERROR"

-- Searches the "canon" IPA consonants for a match
searchCIPA :: Place -> Manner -> Phonation -> Airstream -> Maybe Text
searchCIPA p m h a = snd <$> found where
  found = find (\(Consonant pc mc hc ac, _) -> p == pc && m == mc && h == hc && a == ac) c

-- Decides what IPA symbol to use
retrieveVSymbol :: Height -> Backness -> Roundedness -> Length -> Text
retrieveVSymbol h b r l
    | isJust $ searchVIPA h b r l = fromMaybe "ERROR" (searchVIPA h b r l)
    -- Long("\720") and Short("\774")
    | l == LONG = retrieveVSymbol h b r NORMAL ++ "\720"
    | l == SHORT = retrieveVSymbol h b r NORMAL ++ "\774"
    -- Mid-centralized("\829")
    | h == NEARCLOSE && b == NEARFRONT = retrieveVSymbol CLOSE FRONT r l ++ "\829"
    | h == NEARCLOSE && b == NEARBACK = retrieveVSymbol CLOSE BACK r l ++ "\829"
    | h == NEAROPEN && b == NEARFRONT = retrieveVSymbol OPEN FRONT r l ++ "\829"
    | h == NEAROPEN && b == NEARBACK = retrieveVSymbol OPEN BACK r l ++ "\829"
    | h == MID && b == NEARFRONT = retrieveVSymbol CLOSEMID FRONT r l ++ "\829"
    | h == MID && b == NEARBACK = retrieveVSymbol CLOSEMID BACK r l ++ "\829"
    -- Centralized("\776")
    | b == CENTRAL && r == UNROUNDED && isJust (searchVIPA h NEARFRONT r l) = retrieveVSymbol h NEARFRONT r l ++ "\776"
    | b == CENTRAL && r == ROUNDED && isJust (searchVIPA h NEARBACK r l) = retrieveVSymbol h NEARBACK r l ++ "\776"
    | b == CENTRAL && r == UNROUNDED = retrieveVSymbol h FRONT r l ++ "\776"
    | b == CENTRAL && r == ROUNDED = retrieveVSymbol h BACK r l ++ "\776"
    -- Lowering("\798")
    | h == MID = retrieveVSymbol CLOSEMID b r l ++ "\798"
    | h == NEARCLOSE = retrieveVSymbol OPENMID b r l ++ "\798"
    -- Raising("\797")
    | h == NEAROPEN = retrieveVSymbol OPENMID b r l ++ "\797"
    -- Retracting("\800") and advancing("\799")
    | b == NEARFRONT = retrieveVSymbol h FRONT r l ++ "\800"
    | b == NEARBACK = retrieveVSymbol h BACK r l ++ "\799"
    -- Otherwise error
    | otherwise = "ERROR"

-- Searches the "canon" IPA vowels for a match
searchVIPA :: Height -> Backness -> Roundedness -> Length -> Maybe Text
searchVIPA h b r l = snd <$> found where
  found = find (\(Vowel hv bv rv lv, _) -> h == hv && b == bv && r == rv && l == lv) v

-- Tone letters
writeToneLetterIPA :: Tone -> Text
writeToneLetterIPA t
  | t == NONET =   ""
  | t == TOPT =    "˥"
  | t == HIGHT =   "˦"
  | t == MIDT =    "˧"
  | t == LOWT =    "˨"
  | t == BOTTOMT = "˩"
  | t == FALLT =   "˧˩"
  | t == HFALLT =  "˥˩"
  | t == LFALLT =  "˥˦"
  | t == RISET =   "˧˥"
  | t == HRISET =  "˩˥"
  | t == LRISET =  "˩˨"
  | t == DIPT =    "˥˧˥"
  | t == PEAKT =   "˩˧˩"

writeStressMarkIPA :: Stress -> Text
writeStressMarkIPA s
  | s == NONES = "."
  | s == PRIMARYS = "\712"
  | s == SECONDARYS = "\716"

-- Phoneme data
c :: [(Phoneme, Text)]
c = [-- Nasal Stops
      (Consonant BILABIAL NASAL MODAL PULMONIC, "m")
    , (Consonant LABIODENTAL NASAL MODAL  PULMONIC, "ɱ")
    , (Consonant ALVEOLAR NASAL MODAL  PULMONIC, "n")
    , (Consonant ALVEOLOPALATAL NASAL MODAL  PULMONIC, "ȵ")
    , (Consonant RETROFLEX NASAL MODAL  PULMONIC, "ɳ")
    , (Consonant PALATAL NASAL MODAL PULMONIC, "ɲ")
    , (Consonant VELAR NASAL MODAL PULMONIC, "ŋ")
    , (Consonant UVULAR NASAL MODAL PULMONIC, "ɴ")
    -- (Non-Nasal) Stops
    , (Consonant BILABIAL STOP VOICELESS PULMONIC, "p")
    , (Consonant BILABIAL STOP MODAL PULMONIC, "b")
    , (Consonant ALVEOLAR STOP VOICELESS PULMONIC, "t")
    , (Consonant ALVEOLAR STOP MODAL PULMONIC, "d")
    , (Consonant ALVEOLOPALATAL STOP MODAL PULMONIC, "ȶ")
    , (Consonant ALVEOLOPALATAL STOP VOICELESS PULMONIC, "ȡ")
    , (Consonant RETROFLEX STOP VOICELESS PULMONIC, "ʈ")
    , (Consonant RETROFLEX STOP MODAL PULMONIC, "ɖ")
    , (Consonant PALATAL STOP VOICELESS PULMONIC, "c")
    , (Consonant PALATAL STOP MODAL PULMONIC, "ɟ")
    , (Consonant VELAR STOP VOICELESS PULMONIC, "k")
    , (Consonant VELAR STOP MODAL PULMONIC, "ɡ")
    , (Consonant UVULAR STOP VOICELESS PULMONIC, "q")
    , (Consonant UVULAR STOP MODAL PULMONIC, "ɢ")
    , (Consonant EPIGLOTTAL STOP MODAL PULMONIC, "ʡ")
    , (Consonant GLOTTAL STOP VOICELESS PULMONIC, "ʔ")
    -- Silibant Affricates

    -- (Non-Silibant/Non-Lateral) Affricates

    -- Silibant Fricatives
    , (Consonant ALVEOLAR SILIBANT VOICELESS PULMONIC, "s")
    , (Consonant ALVEOLAR SILIBANT MODAL PULMONIC, "z")
    , (Consonant POSTALVEOLAR SILIBANT VOICELESS PULMONIC, "ʃ")
    , (Consonant POSTALVEOLAR SILIBANT MODAL PULMONIC, "ʒ")
    , (Consonant RETROFLEX SILIBANT VOICELESS PULMONIC, "ʂ")
    , (Consonant RETROFLEX SILIBANT MODAL PULMONIC, "ʐ")
    , (Consonant ALVEOLOPALATAL SILIBANT VOICELESS PULMONIC, "ɕ")
    , (Consonant ALVEOLOPALATAL SILIBANT MODAL PULMONIC, "ʑ")
    -- (Non-Silibant/Non-Lateral) Fricatives
    , (Consonant BILABIAL FRICATIVE VOICELESS PULMONIC, "ɸ")
    , (Consonant BILABIAL FRICATIVE MODAL PULMONIC, "β")
    , (Consonant LABIODENTAL FRICATIVE VOICELESS PULMONIC, "f")
    , (Consonant LABIODENTAL FRICATIVE MODAL PULMONIC, "v")
    , (Consonant DENTAL FRICATIVE VOICELESS PULMONIC, "θ")
    , (Consonant DENTAL FRICATIVE MODAL PULMONIC, "ð")
    , (Consonant PALATAL FRICATIVE VOICELESS PULMONIC, "ç")
    , (Consonant PALATAL FRICATIVE MODAL PULMONIC, "ʝ")
    , (Consonant VELAR FRICATIVE VOICELESS PULMONIC, "x")
    , (Consonant VELAR FRICATIVE MODAL PULMONIC, "ɣ")
    , (Consonant UVULAR FRICATIVE VOICELESS PULMONIC, "χ")
    , (Consonant UVULAR FRICATIVE MODAL PULMONIC, "ʁ")
    , (Consonant PHARYNGEAL FRICATIVE VOICELESS PULMONIC, "ħ")
    , (Consonant PHARYNGEAL FRICATIVE MODAL PULMONIC, "ʕ")
    , (Consonant GLOTTAL FRICATIVE VOICELESS PULMONIC, "h")
    , (Consonant GLOTTAL FRICATIVE MODAL PULMONIC, "ɦ")
    -- Approximants
    , (Consonant LABIODENTAL APPROXIMANT MODAL PULMONIC, "ʋ")
    , (Consonant ALVEOLAR APPROXIMANT MODAL PULMONIC, "ɹ")
    , (Consonant RETROFLEX APPROXIMANT MODAL PULMONIC, "ɻ")
    , (Consonant PALATAL APPROXIMANT MODAL PULMONIC, "j")
    , (Consonant VELAR APPROXIMANT MODAL PULMONIC, "ɰ")
    -- (Non-Lateral) Flaps
    , (Consonant LABIODENTAL FLAP MODAL PULMONIC, "ⱱ")
    , (Consonant ALVEOLAR FLAP MODAL PULMONIC, "ɾ")
    , (Consonant RETROFLEX FLAP MODAL PULMONIC, "ɽ")
    -- Trills
    , (Consonant BILABIAL TRILL MODAL PULMONIC, "ʙ")
    , (Consonant ALVEOLAR TRILL MODAL PULMONIC, "r")
    , (Consonant RETROFLEX TRILL VOICELESS PULMONIC,"ɽ̥͡r̥") --special
    , (Consonant RETROFLEX TRILL MODAL PULMONIC, "ɽ͡r")
    , (Consonant UVULAR TRILL MODAL PULMONIC, "ʀ")
    , (Consonant EPIGLOTTAL TRILL VOICELESS PULMONIC, "ʜ")
    , (Consonant EPIGLOTTAL TRILL MODAL PULMONIC, "ʢ")
    -- Lateral Affricates

    -- Lateral Fricatives
    , (Consonant ALVEOLAR LFRICATIVE VOICELESS PULMONIC, "ɬ")
    , (Consonant ALVEOLAR LFRICATIVE MODAL PULMONIC, "ɮ")
    -- Lateral Approximants
    , (Consonant ALVEOLAR LAPPROXIMANT MODAL PULMONIC, "l")
    , (Consonant RETROFLEX LAPPROXIMANT MODAL PULMONIC, "ɭ")
    , (Consonant PALATAL LAPPROXIMANT MODAL PULMONIC, "ʎ")
    , (Consonant ALVEOLOPALATAL LAPPROXIMANT MODAL PULMONIC, "ȴ")
    , (Consonant VELAR LAPPROXIMANT MODAL PULMONIC, "ʟ")
    -- Lateral Flaps
    , (Consonant ALVEOLAR LFLAP MODAL PULMONIC, "ɺ")
    -- Ejectives

    -- Implosives
    , (Consonant BILABIAL STOP MODAL IMPLOSIVE, "ɓ")
    , (Consonant BILABIAL STOP VOICELESS IMPLOSIVE, "ƥ")
    , (Consonant ALVEOLAR STOP MODAL IMPLOSIVE, "ɗ")
    , (Consonant ALVEOLAR STOP VOICELESS IMPLOSIVE, "ƭ")
    , (Consonant RETROFLEX STOP MODAL IMPLOSIVE, "ᶑ")
    , (Consonant PALATAL STOP MODAL IMPLOSIVE, "ʄ")
    , (Consonant PALATAL STOP VOICELESS IMPLOSIVE, "ƈ")
    , (Consonant VELAR STOP MODAL IMPLOSIVE, "ɠ")
    , (Consonant VELAR STOP VOICELESS IMPLOSIVE, "ƙ")
    , (Consonant UVULAR STOP MODAL IMPLOSIVE, "ʛ")
    , (Consonant UVULAR STOP VOICELESS IMPLOSIVE, "ʠ")
    -- Clicks
    , (Consonant BILABIAL CLICK VOICELESS LINGUAL, "ʘ")
    , (Consonant DENTAL CLICK VOICELESS LINGUAL, "ǀ")
    , (Consonant ALVEOLAR CLICK VOICELESS LINGUAL, "ǃ")
    , (Consonant RETROFLEX CLICK VOICELESS LINGUAL, "‼")
    , (Consonant PALATAL CLICK VOICELESS LINGUAL, "ǂ")
    -- Coarticulated
    , (Consonant (COARTICULATED PALATOALVEOLAR VELAR) FRICATIVE VOICELESS PULMONIC, "ɧ") --this is not used
    , (Consonant (COARTICULATED BILABIAL VELAR) APPROXIMANT VOICELESS PULMONIC, "ʍ")
    , (Consonant (COARTICULATED BILABIAL VELAR) APPROXIMANT MODAL PULMONIC, "w")
    , (Consonant (COARTICULATED ALVEOLAR VELAR) LAPPROXIMANT MODAL PULMONIC, "ɫ")
    , (Consonant (COARTICULATED BILABIAL PALATAL) APPROXIMANT MODAL PULMONIC, "ɥ")
    ]

v :: [(Phoneme, Text)]
v = [-- Close
      (Vowel CLOSE FRONT UNROUNDED NORMAL, "i")
    , (Vowel CLOSE FRONT ROUNDED NORMAL, "y")
    , (Vowel CLOSE CENTRAL UNROUNDED NORMAL, "ɨ")
    , (Vowel CLOSE CENTRAL ROUNDED NORMAL, "ʉ")
    , (Vowel CLOSE BACK UNROUNDED NORMAL, "ɯ")
    , (Vowel CLOSE BACK ROUNDED NORMAL, "u")
    -- Near-close
    , (Vowel NEARCLOSE NEARFRONT UNROUNDED NORMAL, "ɪ")
    , (Vowel NEARCLOSE NEARFRONT ROUNDED NORMAL, "ʏ")
    , (Vowel NEARCLOSE NEARBACK ROUNDED NORMAL, "ʊ")
    -- Close-mid
    , (Vowel CLOSEMID FRONT UNROUNDED NORMAL, "e")
    , (Vowel CLOSEMID FRONT ROUNDED NORMAL, "ø")
    , (Vowel CLOSEMID CENTRAL UNROUNDED NORMAL, "ɘ")
    , (Vowel CLOSEMID CENTRAL ROUNDED NORMAL, "ɵ")
    , (Vowel CLOSEMID BACK UNROUNDED NORMAL, "ɤ")
    , (Vowel CLOSEMID BACK ROUNDED NORMAL, "o")
    -- Mid
    , (Vowel MID CENTRAL UNROUNDED NORMAL, "ə")
    -- Open-mid
    , (Vowel OPENMID FRONT UNROUNDED NORMAL, "ɛ")
    , (Vowel OPENMID FRONT ROUNDED NORMAL, "œ")
    , (Vowel OPENMID CENTRAL UNROUNDED NORMAL, "ɜ")
    , (Vowel OPENMID CENTRAL ROUNDED NORMAL, "ɞ")
    , (Vowel OPENMID BACK UNROUNDED NORMAL, "ʌ")
    , (Vowel OPENMID BACK ROUNDED NORMAL, "ɔ")
    -- Near-open
    , (Vowel NEAROPEN FRONT UNROUNDED NORMAL, "æ")
    , (Vowel NEAROPEN CENTRAL UNROUNDED NORMAL, "ɐ")
    -- Open
    , (Vowel OPEN FRONT UNROUNDED NORMAL, "a")
    , (Vowel OPEN FRONT ROUNDED NORMAL, "ɶ")
    , (Vowel OPEN BACK UNROUNDED NORMAL, "ɑ")
    , (Vowel OPEN BACK ROUNDED NORMAL, "ɒ")
    ]

{-
c :: [[(Phoneme, Text)]]
c = [ [(Consonant BILABIAL NASAL VOICELESS, "m̥"),(Consonant BILABIAL NASAL MODAL, "m"),(Consonant LABIODENTAL NASAL VOICELESS, "ɱ̊"),(Consonant LABIODENTAL NASAL MODAL, "ɱ"),(Consonant DENTAL NASAL VOICELESS, "n̪̊"),(Consonant DENTAL NASAL MODAL, "n̪"),(Consonant ALVEOLAR NASAL VOICELESS, "n̥"),(Consonant ALVEOLAR NASAL MODAL, "n"), (Consonant POSTALVEOLAR NASAL VOICELESS, "n̠̊"),(Consonant POSTALVEOLAR NASAL MODAL, "n̠"),(Consonant RETROFLEX NASAL VOICELESS, "ɳ̊"),(Consonant RETROFLEX NASAL MODAL, "ɳ"),(Consonant ALVEOLOPALATAL NASAL VOICELESS, "ɲ̟̊"),(Consonant ALVEOLOPALATAL NASAL MODAL, "ɲ̟"),(Consonant PALATAL NASAL VOICELESS, "ɲ̊"),(Consonant PALATAL NASAL MODAL, "ɲ"),(Consonant VELAR NASAL VOICELESS, "ŋ̊"),(Consonant VELAR NASAL MODAL, "ŋ"),(Consonant UVULAR NASAL VOICELESS, "ɴ̥"),(Consonant UVULAR NASAL MODAL, "ɴ"),(Consonant PHARYNGEAL NASAL VOICELESS, ""),(Consonant PHARYNGEAL NASAL MODAL, ""),(Consonant GLOTTAL NASAL VOICELESS, ""),(Consonant GLOTTAL NASAL MODAL, "")]
    , [(Consonant BILABIAL STOP VOICELESS, "p"),(Consonant BILABIAL STOP MODAL, "b"),(Consonant LABIODENTAL STOP VOICELESS, "p̪"),(Consonant LABIODENTAL STOP MODAL, "b̪"),(Consonant DENTAL STOP VOICELESS, "t̪"),(Consonant DENTAL STOP MODAL, "d̪"),(Consonant ALVEOLAR STOP VOICELESS, "t"),(Consonant ALVEOLAR STOP MODAL, "d"),(Consonant POSTALVEOLAR STOP VOICELESS, "t̠"),(Consonant POSTALVEOLAR STOP MODAL, "d̠"),(Consonant RETROFLEX STOP VOICELESS, "ʈ"),(Consonant RETROFLEX STOP MODAL, "ɖ"),(Consonant ALVEOLOPALATAL STOP VOICELESS, "c̟"),(Consonant ALVEOLOPALATAL STOP MODAL, "ɟ̟"),(Consonant PALATAL STOP VOICELESS, "c"),(Consonant PALATAL STOP MODAL, "ɟ"),(Consonant VELAR STOP VOICELESS, "k"),(Consonant VELAR STOP MODAL, "ɡ"),(Consonant UVULAR STOP VOICELESS, "q"),(Consonant UVULAR STOP MODAL, "ɢ"),(Consonant EPIGLOTTAL STOP VOICELESS, "ʡ̥"),(Consonant EPIGLOTTAL STOP MODAL, "ʡ"),(Consonant GLOTTAL STOP VOICELESS, "ʔ"),(Consonant GLOTTAL STOP MODAL, "")]
    , [(Consonant BILABIAL AFFRICATE VOICELESS, "p͡ɸ"),(Consonant BILABIAL AFFRICATE MODAL, "b͡β"),(Consonant LABIODENTAL AFFRICATE VOICELESS, "p̪͡f"),(Consonant LABIODENTAL AFFRICATE MODAL, "b̪͡v"),(Consonant DENTAL SAFFRICATE VOICELESS, "t̪͡s̪"),(Consonant DENTAL SAFFRICATE MODAL, "d̪͡z̪"),(Consonant ALVEOLAR SAFFRICATE VOICELESS, "t͡s"),(Consonant ALVEOLAR SAFFRICATE MODAL, "d͡z"),(Consonant POSTALVEOLAR SAFFRICATE VOICELESS, "t̠͡ʃ"),(Consonant POSTALVEOLAR SAFFRICATE MODAL, "d̠͡ʒ"),(Consonant RETROFLEX SAFFRICATE VOICELESS, "ʈ͡ʂ"),(Consonant RETROFLEX SAFFRICATE MODAL, "ɖ͡ʐ"),(Consonant ALVEOLOPALATAL SAFFRICATE VOICELESS, "c̟͡ɕ"),(Consonant ALVEOLOPALATAL SAFFRICATE MODAL, "ɟ̟͡ʑ"),(Consonant PALATAL AFFRICATE VOICELESS, "c͡ç"),(Consonant PALATAL AFFRICATE MODAL, "ɟ͡ʝ"),(Consonant VELAR AFFRICATE VOICELESS, "k͡x"),(Consonant VELAR AFFRICATE MODAL, "ɡ͡ɣ"),(Consonant UVULAR AFFRICATE VOICELESS, "q͡χ"),(Consonant UVULAR AFFRICATE MODAL, "ɢ͡ʁ"),(Consonant PHARYNGEAL AFFRICATE VOICELESS, "ʡ̥͡ħ"),(Consonant PHARYNGEAL AFFRICATE MODAL, "ʡ͡ʕ"),(Consonant GLOTTAL AFFRICATE VOICELESS, "ʔ͡h"),(Consonant GLOTTAL AFFRICATE MODAL, "")]
    , [(Consonant BILABIAL FRICATIVE VOICELESS, "ɸ"),(Consonant BILABIAL FRICATIVE MODAL, "β"),(Consonant LABIODENTAL FRICATIVE VOICELESS, "f"),(Consonant LABIODENTAL FRICATIVE MODAL, "v"),(Consonant PALATAL FRICATIVE VOICELESS, "ç"),(Consonant PALATAL FRICATIVE MODAL, "ʝ"),(Consonant VELAR FRICATIVE VOICELESS, "x"),(Consonant VELAR FRICATIVE MODAL, "ɣ"),(Consonant UVULAR FRICATIVE VOICELESS, "χ"),(Consonant UVULAR FRICATIVE MODAL, "ʁ"),(Consonant PHARYNGEAL FRICATIVE VOICELESS, "ħ"),(Consonant PHARYNGEAL FRICATIVE MODAL, "ʕ"),(Consonant GLOTTAL FRICATIVE VOICELESS, "h"),(Consonant GLOTTAL FRICATIVE MODAL, "ɦ")]
    , [(Consonant DENTAL SILIBANT VOICELESS, "s̪"),(Consonant DENTAL SILIBANT MODAL, "z̪"),(Consonant ALVEOLAR SILIBANT VOICELESS, "s"),(Consonant ALVEOLAR SILIBANT MODAL, "z"),(Consonant POSTALVEOLAR SILIBANT VOICELESS, "ʃ"),(Consonant POSTALVEOLAR SILIBANT MODAL, "ʒ"),(Consonant RETROFLEX SILIBANT VOICELESS, "ʂ"),(Consonant RETROFLEX SILIBANT MODAL, "ʐ"),(Consonant ALVEOLOPALATAL SILIBANT VOICELESS, "ɕ"),(Consonant ALVEOLOPALATAL SILIBANT MODAL, "ʑ")]
    , [(Consonant BILABIAL APPROXIMANT VOICELESS, "ɸ̝"),(Consonant BILABIAL APPROXIMANT MODAL, "β̞"),(Consonant LABIODENTAL APPROXIMANT VOICELESS, "ʋ̥"),(Consonant LABIODENTAL APPROXIMANT MODAL, "ʋ"),(Consonant DENTAL APPROXIMANT VOICELESS, "ð̞̊"),(Consonant DENTAL APPROXIMANT MODAL, "ð̞"),(Consonant ALVEOLAR APPROXIMANT VOICELESS, "ɹ̊"),(Consonant ALVEOLAR APPROXIMANT MODAL, "ɹ"),(Consonant POSTALVEOLAR APPROXIMANT VOICELESS, "ɹ̠̊"),(Consonant POSTALVEOLAR APPROXIMANT MODAL, "ɹ̠"),(Consonant RETROFLEX APPROXIMANT VOICELESS, "ɻ̊"),(Consonant RETROFLEX APPROXIMANT MODAL, "ɻ"),(Consonant ALVEOLOPALATAL APPROXIMANT VOICELESS, "ɻ̠̊"),(Consonant ALVEOLOPALATAL APPROXIMANT MODAL, "ɻ̠"),(Consonant PALATAL APPROXIMANT VOICELESS, "j̊"),(Consonant PALATAL APPROXIMANT MODAL, "j"),(Consonant VELAR APPROXIMANT VOICELESS, "ɰ̊"),(Consonant VELAR APPROXIMANT MODAL, "ɰ"),(Consonant UVULAR APPROXIMANT VOICELESS, "ʁ̞̊"),(Consonant UVULAR APPROXIMANT MODAL, "ʁ̞"),(Consonant PHARYNGEAL APPROXIMANT VOICELESS, "ħ̞"),(Consonant PHARYNGEAL APPROXIMANT MODAL, "ʕ̞"),(Consonant GLOTTAL APPROXIMANT VOICELESS, "h̞"),(Consonant GLOTTAL APPROXIMANT MODAL, "ɦ̞")]
    , [(Consonant BILABIAL FLAP VOICELESS, "ⱱ̟̊"),(Consonant BILABIAL FLAP MODAL, "ⱱ̟"),(Consonant LABIODENTAL FLAP VOICELESS, "ⱱ̥"),(Consonant LABIODENTAL FLAP MODAL, "ⱱ"),(Consonant DENTAL FLAP VOICELESS, "ɾ̪̊ "),(Consonant DENTAL FLAP MODAL, "ɾ̪"),(Consonant ALVEOLAR FLAP VOICELESS, "ɾ̥"),(Consonant ALVEOLAR FLAP MODAL, "ɾ"),(Consonant POSTALVEOLAR FLAP VOICELESS, "ɾ̠̊"),(Consonant POSTALVEOLAR FLAP MODAL, "ɾ̠"),(Consonant RETROFLEX FLAP VOICELESS, "ɽ̊"),(Consonant RETROFLEX FLAP MODAL, "ɽ"),(Consonant ALVEOLOPALATAL FLAP VOICELESS, "ɽ̠̊"),(Consonant ALVEOLOPALATAL FLAP MODAL, "ɽ̠"),(Consonant PALATAL FLAP VOICELESS, "c̆"),(Consonant PALATAL FLAP MODAL, "ɟ̆"),(Consonant VELAR FLAP VOICELESS, ""),(Consonant VELAR FLAP MODAL, ""),(Consonant UVULAR FLAP VOICELESS, "q̆"),(Consonant UVULAR FLAP MODAL, "ɢ̆"),(Consonant EPIGLOTTAL FLAP VOICELESS, ""),(Consonant EPIGLOTTAL FLAP MODAL, "ʡ̮"),(Consonant GLOTTAL FLAP VOICELESS, ""),(Consonant GLOTTAL FLAP MODAL, "")]
    , [(Consonant BILABIAL TRILL VOICELESS, "ʙ̥"),(Consonant BILABIAL TRILL MODAL, "ʙ"),(Consonant LABIODENTAL TRILL VOICELESS, "ʙ̠̊ "),(Consonant LABIODENTAL TRILL MODAL, "ʙ̠"),(Consonant DENTAL TRILL VOICELESS, "r̪̊ "),(Consonant DENTAL TRILL MODAL, "r̪"),(Consonant ALVEOLAR TRILL VOICELESS, "r̥"),(Consonant ALVEOLAR TRILL MODAL, "r"),(Consonant POSTALVEOLAR TRILL VOICELESS, "r̠̊"),(Consonant POSTALVEOLAR TRILL MODAL, "r̠"),(Consonant RETROFLEX TRILL VOICELESS, "ɽ͡r̥"),(Consonant RETROFLEX TRILL MODAL, "ɽ͡r"),(Consonant ALVEOLOPALATAL TRILL VOICELESS, ""),(Consonant ALVEOLOPALATAL TRILL MODAL, ""),(Consonant PALATAL TRILL VOICELESS, ""),(Consonant PALATAL TRILL MODAL, ""),(Consonant VELAR TRILL VOICELESS, ""),(Consonant VELAR TRILL MODAL, ""),(Consonant UVULAR TRILL VOICELESS, "ʀ̥"),(Consonant UVULAR TRILL MODAL, "ʀ"),(Consonant EPIGLOTTAL TRILL VOICELESS, "ʜ"),(Consonant EPIGLOTTAL TRILL MODAL, "ʢ"),(Consonant GLOTTAL TRILL VOICELESS, ""),(Consonant GLOTTAL TRILL MODAL, "")]
    , [(Consonant BILABIAL LAFFRICATE VOICELESS, ""),(Consonant BILABIAL LAFFRICATE MODAL, ""),(Consonant LABIODENTAL LAFFRICATE VOICELESS, ""),(Consonant LABIODENTAL LAFFRICATE MODAL, ""),(Consonant DENTAL LAFFRICATE VOICELESS, "t̪͡ɬ̪̊"),(Consonant DENTAL LAFFRICATE MODAL, "d̪͡ɮ̪"),(Consonant ALVEOLAR LAFFRICATE VOICELESS, "t͡ɬ"),(Consonant ALVEOLAR LAFFRICATE MODAL, "d͡ɮ"),(Consonant POSTALVEOLAR LAFFRICATE VOICELESS, ""),(Consonant POSTALVEOLAR LAFFRICATE MODAL, "d̠͡ɮ̠"),(Consonant RETROFLEX LAFFRICATE VOICELESS, "ʈ͡ɭ̥˔"),(Consonant RETROFLEX LAFFRICATE MODAL, "ɖ͡ɭ˔"),(Consonant ALVEOLOPALATAL LAFFRICATE VOICELESS, "c̟͡ʎ̟̝̊"),(Consonant ALVEOLOPALATAL LAFFRICATE MODAL, "ɟ̟͡ʎ̟̝"),(Consonant PALATAL LAFFRICATE VOICELESS, "c͡ʎ̝̊"),(Consonant PALATAL LAFFRICATE MODAL, "ɟ͡ʎ̝̊"),(Consonant VELAR LAFFRICATE VOICELESS, "k͡ʟ̝̊"),(Consonant VELAR LAFFRICATE MODAL, "ɡ͡ʟ̝"),(Consonant UVULAR LAFFRICATE VOICELESS, "q͡ʟ̝̠̊"),(Consonant UVULAR LAFFRICATE MODAL, "ɢ͡ʟ̝̠"),(Consonant PHARYNGEAL LAFFRICATE VOICELESS, ""),(Consonant PHARYNGEAL LAFFRICATE MODAL, ""),(Consonant GLOTTAL LAFFRICATE VOICELESS, ""),(Consonant GLOTTAL LAFFRICATE MODAL, "")]
    , [(Consonant BILABIAL LFRICATIVE VOICELESS, ""),(Consonant BILABIAL LFRICATIVE MODAL, ""),(Consonant LABIODENTAL LFRICATIVE VOICELESS, ""),(Consonant LABIODENTAL LFRICATIVE MODAL, ""),(Consonant DENTAL LFRICATIVE VOICELESS, "ɬ̪̊"),(Consonant DENTAL LFRICATIVE MODAL, "ɮ̪"),(Consonant ALVEOLAR LFRICATIVE VOICELESS, "ɬ"),(Consonant ALVEOLAR LFRICATIVE MODAL, "ɮ"),(Consonant POSTALVEOLAR LFRICATIVE VOICELESS, "ɬ̠"),(Consonant POSTALVEOLAR LFRICATIVE MODAL, "ɮ̠"),(Consonant RETROFLEX LFRICATIVE VOICELESS, "ɭ̥˔"),(Consonant RETROFLEX LFRICATIVE MODAL, "ɭ˔"),(Consonant ALVEOLOPALATAL LFRICATIVE VOICELESS, "ʎ̟̝̊"),(Consonant ALVEOLOPALATAL LFRICATIVE MODAL, "ʎ̟̝"),(Consonant PALATAL LFRICATIVE VOICELESS, "ʎ̝̊"),(Consonant PALATAL LFRICATIVE MODAL, "ʎ̝̊"),(Consonant VELAR LFRICATIVE VOICELESS, "ʟ̝̊"),(Consonant VELAR LFRICATIVE MODAL, "ʟ̝"),(Consonant UVULAR LFRICATIVE VOICELESS, "ʟ̝̠̊"),(Consonant UVULAR LFRICATIVE MODAL, "ʟ̝̠"),(Consonant PHARYNGEAL LFRICATIVE VOICELESS, ""),(Consonant PHARYNGEAL LFRICATIVE MODAL, ""),(Consonant GLOTTAL LFRICATIVE VOICELESS, ""),(Consonant GLOTTAL LFRICATIVE MODAL, "")]
    , [(Consonant BILABIAL LAPPROXIMANT VOICELESS, ""),(Consonant BILABIAL LAPPROXIMANT MODAL, ""),(Consonant LABIODENTAL LAPPROXIMANT VOICELESS, ""),(Consonant LABIODENTAL LAPPROXIMANT MODAL, ""),(Consonant DENTAL LAPPROXIMANT VOICELESS, "l̪̊"),(Consonant DENTAL LAPPROXIMANT MODAL, "l̪"),(Consonant ALVEOLAR LAPPROXIMANT VOICELESS, "l̥"),(Consonant ALVEOLAR LAPPROXIMANT MODAL, "l"),(Consonant POSTALVEOLAR LAPPROXIMANT VOICELESS, "l̠̊"),(Consonant POSTALVEOLAR LAPPROXIMANT MODAL, "l̠"),(Consonant RETROFLEX LAPPROXIMANT VOICELESS, "ɭ̥"),(Consonant RETROFLEX LAPPROXIMANT MODAL, "ɭ"),(Consonant ALVEOLOPALATAL LAPPROXIMANT VOICELESS, "ʎ̟̊ "),(Consonant ALVEOLOPALATAL LAPPROXIMANT MODAL, "ʎ̟"),(Consonant PALATAL LAPPROXIMANT VOICELESS, "ʎ̥"),(Consonant PALATAL LAPPROXIMANT MODAL, "ʎ"),(Consonant VELAR LAPPROXIMANT VOICELESS, "ʟ̥"),(Consonant VELAR LAPPROXIMANT MODAL, "ʟ"),(Consonant UVULAR LAPPROXIMANT VOICELESS, "ʟ̠̊"),(Consonant UVULAR LAPPROXIMANT MODAL, "ʟ̠"),(Consonant PHARYNGEAL LAPPROXIMANT VOICELESS, ""),(Consonant PHARYNGEAL LAPPROXIMANT MODAL, ""),(Consonant GLOTTAL LAPPROXIMANT VOICELESS, ""),(Consonant GLOTTAL LAPPROXIMANT MODAL, "")]
    , [(Consonant BILABIAL LFLAP VOICELESS, ""),(Consonant BILABIAL LFLAP MODAL, ""),(Consonant LABIODENTAL LFLAP VOICELESS, ""),(Consonant LABIODENTAL LFLAP MODAL, ""),(Consonant DENTAL LFLAP VOICELESS, "ɺ̪̊"),(Consonant DENTAL LFLAP MODAL, "ɺ̪"),(Consonant ALVEOLAR LFLAP VOICELESS, "ɺ̥"),(Consonant ALVEOLAR LFLAP MODAL, "ɺ"),(Consonant POSTALVEOLAR LFLAP VOICELESS, "ɺ̠̊"),(Consonant POSTALVEOLAR LFLAP MODAL, "ɺ̠"),(Consonant RETROFLEX LFLAP VOICELESS, "ɺ̢̊"),(Consonant RETROFLEX LFLAP MODAL, "ɺ̢"),(Consonant ALVEOLOPALATAL LFLAP VOICELESS, "ʎ̯̟̊"),(Consonant ALVEOLOPALATAL LFLAP MODAL, "ʎ̯̟"),(Consonant PALATAL LFLAP VOICELESS, "ʎ̯̊"),(Consonant PALATAL LFLAP MODAL, "ʎ̯"),(Consonant VELAR LFLAP VOICELESS, "ʟ̥̆"),(Consonant VELAR LFLAP MODAL, "ʟ̆"),(Consonant UVULAR LFLAP VOICELESS, "ʟ̠̆̊"),(Consonant UVULAR LFLAP MODAL, "ʟ̠̆"),(Consonant PHARYNGEAL LFLAP VOICELESS, ""),(Consonant PHARYNGEAL LFLAP MODAL, ""),(Consonant GLOTTAL LFLAP VOICELESS, ""),(Consonant GLOTTAL LFLAP MODAL, "")]
    ]

v :: [[(Phoneme, Text)]]
v = [ [(Vowel CLOSE FRONT UNROUNDED NORMAL, "i"),(Vowel CLOSE FRONT ROUNDED NORMAL, "y"),(Vowel CLOSE NEARFRONT UNROUNDED NORMAL, "ɪ̝"),(Vowel CLOSE NEARFRONT ROUNDED NORMAL, "ʏ̝"),(Vowel CLOSE CENTRAL UNROUNDED NORMAL, "ɨ"),(Vowel CLOSE CENTRAL ROUNDED NORMAL, "ʉ"),(Vowel CLOSE NEARBACK UNROUNDED NORMAL, "ɯ̟"),(Vowel CLOSE NEARBACK ROUNDED NORMAL, "ʊ̝"),(Vowel CLOSE BACK UNROUNDED NORMAL, "ɯ"),(Vowel CLOSE BACK ROUNDED NORMAL, "u")]
    , [(Vowel NEARCLOSE FRONT UNROUNDED NORMAL, "ɪ̟"),(Vowel NEARCLOSE FRONT ROUNDED NORMAL, "ʏ̟"),(Vowel NEARCLOSE NEARFRONT UNROUNDED NORMAL, "ɪ"),(Vowel NEARCLOSE NEARFRONT ROUNDED NORMAL, "ʏ"),(Vowel NEARCLOSE CENTRAL UNROUNDED NORMAL, "ɪ̈"),(Vowel NEARCLOSE CENTRAL ROUNDED NORMAL, "ʊ̈"),(Vowel NEARCLOSE NEARBACK UNROUNDED NORMAL, "ɯ̽"),(Vowel NEARCLOSE NEARBACK ROUNDED NORMAL, "ʊ"),(Vowel NEARCLOSE BACK UNROUNDED NORMAL, "ɯ̞"),(Vowel NEARCLOSE BACK ROUNDED NORMAL, "ʊ̠")]
    , [(Vowel CLOSEMID FRONT UNROUNDED NORMAL, "e"),(Vowel CLOSEMID FRONT ROUNDED NORMAL, "ø"),(Vowel CLOSEMID NEARFRONT UNROUNDED NORMAL, "ë"),(Vowel CLOSEMID NEARFRONT ROUNDED NORMAL, "ø̈"),(Vowel CLOSEMID CENTRAL UNROUNDED NORMAL, "ɘ"),(Vowel CLOSEMID CENTRAL ROUNDED NORMAL, "ɵ"),(Vowel CLOSEMID NEARBACK UNROUNDED NORMAL, "ɤ̈"),(Vowel CLOSEMID NEARBACK ROUNDED NORMAL, "ö"),(Vowel CLOSEMID BACK UNROUNDED NORMAL, "ɤ"),(Vowel CLOSEMID BACK ROUNDED NORMAL, "o")]
    , [(Vowel MID FRONT UNROUNDED NORMAL, "e̞"),(Vowel MID FRONT ROUNDED NORMAL, "ø̞"),(Vowel MID NEARFRONT UNROUNDED NORMAL, "ë̞"),(Vowel MID NEARFRONT ROUNDED NORMAL, "ø̞̈"),(Vowel MID CENTRAL UNROUNDED NORMAL, "ə"),(Vowel MID CENTRAL ROUNDED NORMAL, "ɵ̞"),(Vowel MID NEARBACK UNROUNDED NORMAL, "ɤ̞̈"),(Vowel MID NEARBACK ROUNDED NORMAL, "ö̞"),(Vowel MID BACK UNROUNDED NORMAL, "ɤ̞"),(Vowel MID BACK ROUNDED NORMAL, "o̞")]
    , [(Vowel OPENMID FRONT UNROUNDED NORMAL, "ɛ"),(Vowel OPENMID FRONT ROUNDED NORMAL, "œ"),(Vowel OPENMID NEARFRONT UNROUNDED NORMAL, "ɛ̈"),(Vowel OPENMID NEARFRONT ROUNDED NORMAL, "œ̈"),(Vowel OPENMID CENTRAL UNROUNDED NORMAL, "ɜ"),(Vowel OPENMID CENTRAL ROUNDED NORMAL, "ɞ"),(Vowel OPENMID NEARBACK UNROUNDED NORMAL, "ʌ̈"),(Vowel OPENMID NEARBACK ROUNDED NORMAL, "ɔ̈"),(Vowel OPENMID BACK UNROUNDED NORMAL, "ʌ"),(Vowel OPENMID BACK ROUNDED NORMAL, "ɔ")]
    , [(Vowel NEAROPEN FRONT UNROUNDED NORMAL, "æ"),(Vowel NEAROPEN FRONT ROUNDED NORMAL, "œ̞"),(Vowel NEAROPEN NEARFRONT UNROUNDED NORMAL, "a̽"),(Vowel NEAROPEN NEARFRONT ROUNDED NORMAL, "ɶ̽"),(Vowel NEAROPEN CENTRAL UNROUNDED NORMAL, "ɐ"),(Vowel NEAROPEN CENTRAL ROUNDED NORMAL, "ɞ̞"),(Vowel NEAROPEN NEARBACK UNROUNDED NORMAL, "ɑ̽"),(Vowel NEAROPEN NEARBACK ROUNDED NORMAL, "ɒ̽"),(Vowel NEAROPEN BACK UNROUNDED NORMAL, "ʌ̞"),(Vowel NEAROPEN BACK ROUNDED NORMAL, "ɔ̞")]
    , [(Vowel OPEN FRONT UNROUNDED NORMAL, "a"),(Vowel OPEN FRONT ROUNDED NORMAL, "ɶ"),(Vowel OPEN NEARFRONT UNROUNDED NORMAL, "a̠"),(Vowel OPEN NEARFRONT ROUNDED NORMAL, "ɶ̠"),(Vowel OPEN CENTRAL UNROUNDED NORMAL, "ä"),(Vowel OPEN CENTRAL ROUNDED NORMAL, "ɒ̈"),(Vowel OPEN NEARBACK UNROUNDED NORMAL, "ɑ̟"),(Vowel OPEN NEARBACK ROUNDED NORMAL, "ɒ̟"),(Vowel OPEN BACK UNROUNDED NORMAL, "ɑ"),(Vowel OPEN BACK ROUNDED NORMAL, "ɒ")]
    ]
-}
