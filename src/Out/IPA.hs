module Out.IPA
( parsePhonemeIPA
) where

import Data.Phoneme

-- parse phoneme into a string
parsePhonemeIPA :: Phoneme -> String
parsePhonemeIPA Blank = ""
parsePhonemeIPA (Consonant p m h) = retrieveCSymbol p m h
parsePhonemeIPA (Vowel h b r l t) = retrieveVSymbol h b r l t
parsePhonemeIPA (Diphthong h1 b1 r1 h2 b2 r2 l t) = retrieveVSymbol h1 b1 r1 l t ++ retrieveVSymbol h2 b2 r2 l t ++ "\815"

-- Decides what IPA symbol to use, essentially
retrieveCSymbol :: Place -> Manner -> Phonation -> String
retrieveCSymbol p m h
    | not.null $ searchCIPA p m h = snd $ head $ searchCIPA p m h
    -- phonation
    | h == CREAKY = retrieveCSymbol p m MODAL ++ "\816"
    | h == STIFF = retrieveCSymbol p m MODAL ++ "\812"
    | h == SLACK = retrieveCSymbol p m MODAL ++ "\805"
    | h == BREATHY = retrieveCSymbol p m MODAL ++ "\804"
    | h == ASPIRATED  = retrieveCSymbol p m MODAL ++ "\688"
    -- place of articulation
    | p == LABIAL = retrieveCSymbol BILABIAL m h
    | p == CORONAL || p == DENTIALVEOLAR = retrieveCSymbol ALVEOLAR m h
    | (p == DORSAL) && (not . null $ searchCIPA VELAR m h) = retrieveCSymbol VELAR m h
    | (p == DORSAL) && (not . null $ searchCIPA UVULAR m h) = retrieveCSymbol UVULAR m h
    | (p == LARYNGEAL) && (not . null $ searchCIPA GLOTTAL m h) = retrieveCSymbol GLOTTAL m h
    | (p == LARYNGEAL) && (not . null $ searchCIPA EPIGLOTTAL m h) = retrieveCSymbol EPIGLOTTAL m h
    | (p == LARYNGEAL) && (not . null $ searchCIPA PHARYNGEAL m h) = retrieveCSymbol PHARYNGEAL m h
    | (p == EPIPHARYNGEAL) && (not . null $ searchCIPA EPIGLOTTAL m h) = retrieveCSymbol EPIGLOTTAL m h
    | (p == EPIPHARYNGEAL) && (not . null $ searchCIPA PHARYNGEAL m h) = retrieveCSymbol PHARYNGEAL m h
    | (p == EPIGLOTTAL) && (not . null $ searchCIPA PHARYNGEAL m h) = retrieveCSymbol PHARYNGEAL m h ++ "\799"
    | (p == PHARYNGEAL) && (not . null $ searchCIPA EPIGLOTTAL m h) = retrieveCSymbol EPIGLOTTAL m h ++ "\800"
    -- manner of articulation
    | m == FRICATIVE  = retrieveCSymbol p APPROXIMANT h ++ "\799"
    | m == SAFFRICATE = retrieveCSymbol p STOP h ++ "\865" ++ retrieveCSymbol p SILIBANT h
    | m == AFFRICATE = retrieveCSymbol p STOP h ++ "\865" ++ retrieveCSymbol p FRICATIVE h
    | m == LAFFRICATE = retrieveCSymbol p STOP h ++ "\865" ++ retrieveCSymbol p LFRICATIVE h
    -- otherwise
    | otherwise = "ERROR"

-- Searches the "canon" IPA consonants for a match
searchCIPA :: Place -> Manner -> Phonation -> [(Phoneme, String)]
searchCIPA p m h = filt where
  cons = filter (not . null . snd) $ concat c
  filt = filter (\(Consonant pc mc hc, _) -> p == pc && m == mc && h == hc) cons


-- Decides what IPA symbol to use
retrieveVSymbol :: Height -> Backness -> Roundedness -> Length -> Tone -> String
retrieveVSymbol h b r l t
    | not.null $ searchVIPA h b r l t = snd $ head $ searchVIPA h b r l t
    | r == DEFAULT && ((b == FRONT) || (b == NEARFRONT)) = retrieveVSymbol h b ROUNDED l t
    | r == DEFAULT = retrieveVSymbol h b UNROUNDED l t
    | t == TOPT = retrieveVSymbol h b r l NONET ++ "\779"
    | t == HIGHT = retrieveVSymbol h b r l NONET ++ "\769"
    | t == MIDT = retrieveVSymbol h b r l NONET ++ "\772"
    | t == LOWT = retrieveVSymbol h b r l NONET ++ "\768"
    | t == BOTTOMT = retrieveVSymbol h b r l NONET ++ "\783"
    | t == FALLT = retrieveVSymbol h b r l NONET ++ "\770"
    | t == HFALLT = retrieveVSymbol h b r l NONET ++ "\7623"
    | t == LFALLT = retrieveVSymbol h b r l NONET ++ "\7622"
    | t == RISET = retrieveVSymbol h b r l NONET ++ "\780"
    | t == HRISET = retrieveVSymbol h b r l NONET ++ "\7620"
    | t == LRISET = retrieveVSymbol h b r l NONET ++ "\7621"
    | t == DIPT = retrieveVSymbol h b r l NONET ++ "\7625"
    | t == PEAKT = retrieveVSymbol h b r l NONET ++ "\7624"
    | l == LONG = retrieveVSymbol h b r NORMAL t ++ "\720"
    | l == SHORT = retrieveVSymbol h b r NORMAL t ++ "\774"
    | otherwise = "ERROR"

-- Searches the "canon" IPA vowels for a match
searchVIPA :: Height -> Backness -> Roundedness -> Length -> Tone -> [(Phoneme, String)]
searchVIPA h b r l t = filt where
  vows = filter (not . null . snd) $ concat v
  filt = filter (\(Vowel hv bv rv lv tv, _) -> h == hv && b == bv && r == rv && l == lv && t == tv) vows


-- Phoneme data
c = [ [(Consonant BILABIAL NASAL VOICELESS, "m̥"),(Consonant BILABIAL NASAL MODAL, "m"),(Consonant LABIODENTAL NASAL VOICELESS, "ɱ̊"),(Consonant LABIODENTAL NASAL MODAL, "ɱ"),(Consonant DENTAL NASAL VOICELESS, "n̪̊"),(Consonant DENTAL NASAL MODAL, "n̪"),(Consonant ALVEOLAR NASAL VOICELESS, "n̥"),(Consonant ALVEOLAR NASAL MODAL, "n"), (Consonant POSTALVEOLAR NASAL VOICELESS, "n̠̊"),(Consonant POSTALVEOLAR NASAL MODAL, "n̠"),(Consonant RETROFLEX NASAL VOICELESS, "ɳ̊"),(Consonant RETROFLEX NASAL MODAL, "ɳ"),(Consonant ALVEOLOPALATAL NASAL VOICELESS, "ɲ̟̊"),(Consonant ALVEOLOPALATAL NASAL MODAL, "ɲ̟"),(Consonant PALATAL NASAL VOICELESS, "ɲ̊"),(Consonant PALATAL NASAL MODAL, "ɲ"),(Consonant VELAR NASAL VOICELESS, "ŋ̊"),(Consonant VELAR NASAL MODAL, "ŋ"),(Consonant UVULAR NASAL VOICELESS, "ɴ̥"),(Consonant UVULAR NASAL MODAL, "ɴ"),(Consonant PHARYNGEAL NASAL VOICELESS, []),(Consonant PHARYNGEAL NASAL MODAL, []),(Consonant GLOTTAL NASAL VOICELESS, []),(Consonant GLOTTAL NASAL MODAL, [])]
    , [(Consonant BILABIAL STOP VOICELESS, "p"),(Consonant BILABIAL STOP MODAL, "b"),(Consonant LABIODENTAL STOP VOICELESS, "p̪"),(Consonant LABIODENTAL STOP MODAL, "b̪"),(Consonant DENTAL STOP VOICELESS, "t̪"),(Consonant DENTAL STOP MODAL, "d̪"),(Consonant ALVEOLAR STOP VOICELESS, "t"),(Consonant ALVEOLAR STOP MODAL, "d"),(Consonant POSTALVEOLAR STOP VOICELESS, "t̠"),(Consonant POSTALVEOLAR STOP MODAL, "d̠"),(Consonant RETROFLEX STOP VOICELESS, "ʈ"),(Consonant RETROFLEX STOP MODAL, "ɖ"),(Consonant ALVEOLOPALATAL STOP VOICELESS, "c̟"),(Consonant ALVEOLOPALATAL STOP MODAL, "ɟ̟"),(Consonant PALATAL STOP VOICELESS, "c"),(Consonant PALATAL STOP MODAL, "ɟ"),(Consonant VELAR STOP VOICELESS, "k"),(Consonant VELAR STOP MODAL, "ɡ"),(Consonant UVULAR STOP VOICELESS, "q"),(Consonant UVULAR STOP MODAL, "ɢ"),(Consonant EPIGLOTTAL STOP VOICELESS, "ʡ̥"),(Consonant EPIGLOTTAL STOP MODAL, "ʡ"),(Consonant GLOTTAL STOP VOICELESS, "ʔ"),(Consonant GLOTTAL STOP MODAL, [])]
    , [(Consonant BILABIAL AFFRICATE VOICELESS, "p͡ɸ"),(Consonant BILABIAL AFFRICATE MODAL, "b͡β"),(Consonant LABIODENTAL AFFRICATE VOICELESS, "p̪͡f"),(Consonant LABIODENTAL AFFRICATE MODAL, "b̪͡v"),(Consonant DENTAL SAFFRICATE VOICELESS, "t̪͡s̪"),(Consonant DENTAL SAFFRICATE MODAL, "d̪͡z̪"),(Consonant ALVEOLAR SAFFRICATE VOICELESS, "t͡s"),(Consonant ALVEOLAR SAFFRICATE MODAL, "d͡z"),(Consonant POSTALVEOLAR SAFFRICATE VOICELESS, "t̠͡ʃ"),(Consonant POSTALVEOLAR SAFFRICATE MODAL, "d̠͡ʒ"),(Consonant RETROFLEX SAFFRICATE VOICELESS, "ʈ͡ʂ"),(Consonant RETROFLEX SAFFRICATE MODAL, "ɖ͡ʐ"),(Consonant ALVEOLOPALATAL SAFFRICATE VOICELESS, "c̟͡ɕ"),(Consonant ALVEOLOPALATAL SAFFRICATE MODAL, "ɟ̟͡ʑ"),(Consonant PALATAL AFFRICATE VOICELESS, "c͡ç"),(Consonant PALATAL AFFRICATE MODAL, "ɟ͡ʝ"),(Consonant VELAR AFFRICATE VOICELESS, "k͡x"),(Consonant VELAR AFFRICATE MODAL, "ɡ͡ɣ"),(Consonant UVULAR AFFRICATE VOICELESS, "q͡χ"),(Consonant UVULAR AFFRICATE MODAL, "ɢ͡ʁ"),(Consonant PHARYNGEAL AFFRICATE VOICELESS, "ʡ̥͡ħ"),(Consonant PHARYNGEAL AFFRICATE MODAL, "ʡ͡ʕ"),(Consonant GLOTTAL AFFRICATE VOICELESS, "ʔ͡h"),(Consonant GLOTTAL AFFRICATE MODAL, [])]
    , [(Consonant BILABIAL FRICATIVE VOICELESS, "ɸ"),(Consonant BILABIAL FRICATIVE MODAL, "β"),(Consonant LABIODENTAL FRICATIVE VOICELESS, "f"),(Consonant LABIODENTAL FRICATIVE MODAL, "v"),(Consonant DENTAL SILIBANT VOICELESS, "s̪"),(Consonant DENTAL SILIBANT MODAL, "z̪"),(Consonant ALVEOLAR SILIBANT VOICELESS, "s"),(Consonant ALVEOLAR SILIBANT MODAL, "z"),(Consonant POSTALVEOLAR SILIBANT VOICELESS, "ʃ"),(Consonant POSTALVEOLAR SILIBANT MODAL, "ʒ"),(Consonant RETROFLEX SILIBANT VOICELESS, "ʂ"),(Consonant RETROFLEX SILIBANT MODAL, "ʐ"),(Consonant ALVEOLOPALATAL SILIBANT VOICELESS, "ɕ"),(Consonant ALVEOLOPALATAL SILIBANT MODAL, "ʑ"),(Consonant PALATAL FRICATIVE VOICELESS, "ç"),(Consonant PALATAL FRICATIVE MODAL, "ʝ"),(Consonant VELAR FRICATIVE VOICELESS, "x"),(Consonant VELAR FRICATIVE MODAL, "ɣ"),(Consonant UVULAR FRICATIVE VOICELESS, "χ"),(Consonant UVULAR FRICATIVE MODAL, "ʁ"),(Consonant PHARYNGEAL FRICATIVE VOICELESS, "ħ"),(Consonant PHARYNGEAL FRICATIVE MODAL, "ʕ"),(Consonant GLOTTAL FRICATIVE VOICELESS, "h"),(Consonant GLOTTAL FRICATIVE MODAL, "ɦ")]
    , [(Consonant BILABIAL APPROXIMANT VOICELESS, "ɸ̝"),(Consonant BILABIAL APPROXIMANT MODAL, "β̞"),(Consonant LABIODENTAL APPROXIMANT VOICELESS, "ʋ̥"),(Consonant LABIODENTAL APPROXIMANT MODAL, "ʋ"),(Consonant DENTAL APPROXIMANT VOICELESS, "ð̞̊"),(Consonant DENTAL APPROXIMANT MODAL, "ð̞"),(Consonant ALVEOLAR APPROXIMANT VOICELESS, "ɹ̊"),(Consonant ALVEOLAR APPROXIMANT MODAL, "ɹ"),(Consonant POSTALVEOLAR APPROXIMANT VOICELESS, "ɹ̠̊"),(Consonant POSTALVEOLAR APPROXIMANT MODAL, "ɹ̠"),(Consonant RETROFLEX APPROXIMANT VOICELESS, "ɻ̊"),(Consonant RETROFLEX APPROXIMANT MODAL, "ɻ"),(Consonant ALVEOLOPALATAL APPROXIMANT VOICELESS, "ɻ̠̊"),(Consonant ALVEOLOPALATAL APPROXIMANT MODAL, "ɻ̠"),(Consonant PALATAL APPROXIMANT VOICELESS, "j̊"),(Consonant PALATAL APPROXIMANT MODAL, "j"),(Consonant VELAR APPROXIMANT VOICELESS, "ɰ̊"),(Consonant VELAR APPROXIMANT MODAL, "ɰ"),(Consonant UVULAR APPROXIMANT VOICELESS, "ʁ̞̊"),(Consonant UVULAR APPROXIMANT MODAL, "ʁ̞"),(Consonant PHARYNGEAL APPROXIMANT VOICELESS, "ħ̞"),(Consonant PHARYNGEAL APPROXIMANT MODAL, "ʕ̞"),(Consonant GLOTTAL APPROXIMANT VOICELESS, "h̞"),(Consonant GLOTTAL APPROXIMANT MODAL, "ɦ̞")]
    , [(Consonant BILABIAL FLAP VOICELESS, "ⱱ̟̊"),(Consonant BILABIAL FLAP MODAL, "ⱱ̟"),(Consonant LABIODENTAL FLAP VOICELESS, "ⱱ̥"),(Consonant LABIODENTAL FLAP MODAL, "ⱱ"),(Consonant DENTAL FLAP VOICELESS, "ɾ̪̊ "),(Consonant DENTAL FLAP MODAL, "ɾ̪"),(Consonant ALVEOLAR FLAP VOICELESS, "ɾ̥"),(Consonant ALVEOLAR FLAP MODAL, "ɾ"),(Consonant POSTALVEOLAR FLAP VOICELESS, "ɾ̠̊"),(Consonant POSTALVEOLAR FLAP MODAL, "ɾ̠"),(Consonant RETROFLEX FLAP VOICELESS, "ɽ̊"),(Consonant RETROFLEX FLAP MODAL, "ɽ"),(Consonant ALVEOLOPALATAL FLAP VOICELESS, "ɽ̠̊"),(Consonant ALVEOLOPALATAL FLAP MODAL, "ɽ̠"),(Consonant PALATAL FLAP VOICELESS, "c̆"),(Consonant PALATAL FLAP MODAL, "ɟ̆"),(Consonant VELAR FLAP VOICELESS, []),(Consonant VELAR FLAP MODAL, []),(Consonant UVULAR FLAP VOICELESS, "q̆"),(Consonant UVULAR FLAP MODAL, "ɢ̆"),(Consonant EPIGLOTTAL FLAP VOICELESS, []),(Consonant EPIGLOTTAL FLAP MODAL, "ʡ̮"),(Consonant GLOTTAL FLAP VOICELESS, []),(Consonant GLOTTAL FLAP MODAL, [])]
    , [(Consonant BILABIAL TRILL VOICELESS, "ʙ̥"),(Consonant BILABIAL TRILL MODAL, "ʙ"),(Consonant LABIODENTAL TRILL VOICELESS, "ʙ̠̊ "),(Consonant LABIODENTAL TRILL MODAL, "ʙ̠"),(Consonant DENTAL TRILL VOICELESS, "r̪̊ "),(Consonant DENTAL TRILL MODAL, "r̪"),(Consonant ALVEOLAR TRILL VOICELESS, "r̥"),(Consonant ALVEOLAR TRILL MODAL, "r"),(Consonant POSTALVEOLAR TRILL VOICELESS, "r̠̊"),(Consonant POSTALVEOLAR TRILL MODAL, "r̠"),(Consonant RETROFLEX TRILL VOICELESS, "ɽ͡r̥"),(Consonant RETROFLEX TRILL MODAL, "ɽ͡r"),(Consonant ALVEOLOPALATAL TRILL VOICELESS, "[]"),(Consonant ALVEOLOPALATAL TRILL MODAL, "[]"),(Consonant PALATAL TRILL VOICELESS, "[]"),(Consonant PALATAL TRILL MODAL, "[]"),(Consonant VELAR TRILL VOICELESS, []),(Consonant VELAR TRILL MODAL, []),(Consonant UVULAR TRILL VOICELESS, "ʀ̥"),(Consonant UVULAR TRILL MODAL, "ʀ"),(Consonant EPIGLOTTAL TRILL VOICELESS, "ʜ"),(Consonant EPIGLOTTAL TRILL MODAL, "ʢ"),(Consonant GLOTTAL TRILL VOICELESS, []),(Consonant GLOTTAL TRILL MODAL, [])]
    , [(Consonant BILABIAL LAFFRICATE VOICELESS, []),(Consonant BILABIAL LAFFRICATE MODAL, []),(Consonant LABIODENTAL LAFFRICATE VOICELESS, []),(Consonant LABIODENTAL LAFFRICATE MODAL, []),(Consonant DENTAL LAFFRICATE VOICELESS, "t̪͡ɬ̪̊"),(Consonant DENTAL LAFFRICATE MODAL, "d̪͡ɮ̪"),(Consonant ALVEOLAR LAFFRICATE VOICELESS, "t͡ɬ"),(Consonant ALVEOLAR LAFFRICATE MODAL, "d͡ɮ"),(Consonant POSTALVEOLAR LAFFRICATE VOICELESS, ""),(Consonant POSTALVEOLAR LAFFRICATE MODAL, "d̠͡ɮ̠"),(Consonant RETROFLEX LAFFRICATE VOICELESS, "ʈ͡ɭ̥˔"),(Consonant RETROFLEX LAFFRICATE MODAL, "ɖ͡ɭ˔"),(Consonant ALVEOLOPALATAL LAFFRICATE VOICELESS, "c̟͡ʎ̟̝̊"),(Consonant ALVEOLOPALATAL LAFFRICATE MODAL, "ɟ̟͡ʎ̟̝"),(Consonant PALATAL LAFFRICATE VOICELESS, "c͡ʎ̝̊"),(Consonant PALATAL LAFFRICATE MODAL, "ɟ͡ʎ̝̊"),(Consonant VELAR LAFFRICATE VOICELESS, "k͡ʟ̝̊"),(Consonant VELAR LAFFRICATE MODAL, "ɡ͡ʟ̝"),(Consonant UVULAR LAFFRICATE VOICELESS, "q͡ʟ̝̠̊"),(Consonant UVULAR LAFFRICATE MODAL, "ɢ͡ʟ̝̠"),(Consonant PHARYNGEAL LAFFRICATE VOICELESS, []),(Consonant PHARYNGEAL LAFFRICATE MODAL, []),(Consonant GLOTTAL LAFFRICATE VOICELESS, []),(Consonant GLOTTAL LAFFRICATE MODAL, [])]
    , [(Consonant BILABIAL LFRICATIVE VOICELESS, []),(Consonant BILABIAL LFRICATIVE MODAL, []),(Consonant LABIODENTAL LFRICATIVE VOICELESS, []),(Consonant LABIODENTAL LFRICATIVE MODAL, []),(Consonant DENTAL LFRICATIVE VOICELESS, "ɬ̪̊"),(Consonant DENTAL LFRICATIVE MODAL, "ɮ̪"),(Consonant ALVEOLAR LFRICATIVE VOICELESS, "ɬ"),(Consonant ALVEOLAR LFRICATIVE MODAL, "ɮ"),(Consonant POSTALVEOLAR LFRICATIVE VOICELESS, "ɬ̠"),(Consonant POSTALVEOLAR LFRICATIVE MODAL, "ɮ̠"),(Consonant RETROFLEX LFRICATIVE VOICELESS, "ɭ̥˔"),(Consonant RETROFLEX LFRICATIVE MODAL, "ɭ˔"),(Consonant ALVEOLOPALATAL LFRICATIVE VOICELESS, "ʎ̟̝̊"),(Consonant ALVEOLOPALATAL LFRICATIVE MODAL, "ʎ̟̝"),(Consonant PALATAL LFRICATIVE VOICELESS, "ʎ̝̊"),(Consonant PALATAL LFRICATIVE MODAL, "ʎ̝̊"),(Consonant VELAR LFRICATIVE VOICELESS, "ʟ̝̊"),(Consonant VELAR LFRICATIVE MODAL, "ʟ̝"),(Consonant UVULAR LFRICATIVE VOICELESS, "ʟ̝̠̊"),(Consonant UVULAR LFRICATIVE MODAL, "ʟ̝̠"),(Consonant PHARYNGEAL LFRICATIVE VOICELESS, []),(Consonant PHARYNGEAL LFRICATIVE MODAL, []),(Consonant GLOTTAL LFRICATIVE VOICELESS, []),(Consonant GLOTTAL LFRICATIVE MODAL, [])]
    , [(Consonant BILABIAL LAPPROXIMANT VOICELESS, []),(Consonant BILABIAL LAPPROXIMANT MODAL, []),(Consonant LABIODENTAL LAPPROXIMANT VOICELESS, []),(Consonant LABIODENTAL LAPPROXIMANT MODAL, []),(Consonant DENTAL LAPPROXIMANT VOICELESS, "l̪̊"),(Consonant DENTAL LAPPROXIMANT MODAL, "l̪"),(Consonant ALVEOLAR LAPPROXIMANT VOICELESS, "l̥"),(Consonant ALVEOLAR LAPPROXIMANT MODAL, "l"),(Consonant POSTALVEOLAR LAPPROXIMANT VOICELESS, "l̠̊"),(Consonant POSTALVEOLAR LAPPROXIMANT MODAL, "l̠"),(Consonant RETROFLEX LAPPROXIMANT VOICELESS, "ɭ̥"),(Consonant RETROFLEX LAPPROXIMANT MODAL, "ɭ"),(Consonant ALVEOLOPALATAL LAPPROXIMANT VOICELESS, "ʎ̟̊ "),(Consonant ALVEOLOPALATAL LAPPROXIMANT MODAL, "ʎ̟"),(Consonant PALATAL LAPPROXIMANT VOICELESS, "ʎ̥"),(Consonant PALATAL LAPPROXIMANT MODAL, "ʎ"),(Consonant VELAR LAPPROXIMANT VOICELESS, "ʟ̥"),(Consonant VELAR LAPPROXIMANT MODAL, "ʟ"),(Consonant UVULAR LAPPROXIMANT VOICELESS, "ʟ̠̊"),(Consonant UVULAR LAPPROXIMANT MODAL, "ʟ̠"),(Consonant PHARYNGEAL LAPPROXIMANT VOICELESS, []),(Consonant PHARYNGEAL LAPPROXIMANT MODAL, []),(Consonant GLOTTAL LAPPROXIMANT VOICELESS, []),(Consonant GLOTTAL LAPPROXIMANT MODAL, [])]
    , [(Consonant BILABIAL LFLAP VOICELESS, []),(Consonant BILABIAL LFLAP MODAL, []),(Consonant LABIODENTAL LFLAP VOICELESS, []),(Consonant LABIODENTAL LFLAP MODAL, []),(Consonant DENTAL LFLAP VOICELESS, "ɺ̪̊"),(Consonant DENTAL LFLAP MODAL, "ɺ̪"),(Consonant ALVEOLAR LFLAP VOICELESS, "ɺ̥"),(Consonant ALVEOLAR LFLAP MODAL, "ɺ"),(Consonant POSTALVEOLAR LFLAP VOICELESS, "ɺ̠̊"),(Consonant POSTALVEOLAR LFLAP MODAL, "ɺ̠"),(Consonant RETROFLEX LFLAP VOICELESS, "ɺ̢̊"),(Consonant RETROFLEX LFLAP MODAL, "ɺ̢"),(Consonant ALVEOLOPALATAL LFLAP VOICELESS, "ʎ̯̟̊"),(Consonant ALVEOLOPALATAL LFLAP MODAL, "ʎ̯̟"),(Consonant PALATAL LFLAP VOICELESS, "ʎ̯̊"),(Consonant PALATAL LFLAP MODAL, "ʎ̯"),(Consonant VELAR LFLAP VOICELESS, "ʟ̥̆"),(Consonant VELAR LFLAP MODAL, "ʟ̆"),(Consonant UVULAR LFLAP VOICELESS, "ʟ̠̆̊"),(Consonant UVULAR LFLAP MODAL, "ʟ̠̆"),(Consonant PHARYNGEAL LFLAP VOICELESS, []),(Consonant PHARYNGEAL LFLAP MODAL, []),(Consonant GLOTTAL LFLAP VOICELESS, []),(Consonant GLOTTAL LFLAP MODAL, [])]
    ]

v = [ [(Vowel CLOSE FRONT UNROUNDED NORMAL NONET, "i"),(Vowel CLOSE FRONT ROUNDED NORMAL NONET, "y"),(Vowel CLOSE NEARFRONT UNROUNDED NORMAL NONET, "ɪ̝"),(Vowel CLOSE NEARFRONT ROUNDED NORMAL NONET, "ʏ̝"),(Vowel CLOSE CENTRAL UNROUNDED NORMAL NONET, "ɨ"),(Vowel CLOSE CENTRAL ROUNDED NORMAL NONET, "ʉ"),(Vowel CLOSE NEARBACK UNROUNDED NORMAL NONET, "ɯ̟"),(Vowel CLOSE NEARBACK ROUNDED NORMAL NONET, "ʊ̝"),(Vowel CLOSE BACK UNROUNDED NORMAL NONET, "ɯ"),(Vowel CLOSE BACK ROUNDED NORMAL NONET, "u")]
    , [(Vowel NEARCLOSE FRONT UNROUNDED NORMAL NONET, "ɪ̟"),(Vowel NEARCLOSE FRONT ROUNDED NORMAL NONET, "ʏ̟"),(Vowel NEARCLOSE NEARFRONT UNROUNDED NORMAL NONET, "ɪ"),(Vowel NEARCLOSE NEARFRONT ROUNDED NORMAL NONET, "ʏ"),(Vowel NEARCLOSE CENTRAL UNROUNDED NORMAL NONET, "ɪ̈"),(Vowel NEARCLOSE CENTRAL ROUNDED NORMAL NONET, "ʊ̈"),(Vowel NEARCLOSE NEARBACK UNROUNDED NORMAL NONET, "ɯ̽"),(Vowel NEARCLOSE NEARBACK ROUNDED NORMAL NONET, "ʊ"),(Vowel NEARCLOSE BACK UNROUNDED NORMAL NONET, "ɯ̞"),(Vowel NEARCLOSE BACK ROUNDED NORMAL NONET, "ʊ̠")]
    , [(Vowel CLOSEMID FRONT UNROUNDED NORMAL NONET, "e"),(Vowel CLOSEMID FRONT ROUNDED NORMAL NONET, "ø"),(Vowel CLOSEMID NEARFRONT UNROUNDED NORMAL NONET, "ë"),(Vowel CLOSEMID NEARFRONT ROUNDED NORMAL NONET, "ø̈"),(Vowel CLOSEMID CENTRAL UNROUNDED NORMAL NONET, "ɘ"),(Vowel CLOSEMID CENTRAL ROUNDED NORMAL NONET, "ɵ"),(Vowel CLOSEMID NEARBACK UNROUNDED NORMAL NONET, "ɤ̈"),(Vowel CLOSEMID NEARBACK ROUNDED NORMAL NONET, "ö"),(Vowel CLOSEMID BACK UNROUNDED NORMAL NONET, "ɤ"),(Vowel CLOSEMID BACK ROUNDED NORMAL NONET, "o")]
    , [(Vowel MID FRONT UNROUNDED NORMAL NONET, "e̞"),(Vowel MID FRONT ROUNDED NORMAL NONET, "ø̞"),(Vowel MID NEARFRONT UNROUNDED NORMAL NONET, "ë̞"),(Vowel MID NEARFRONT ROUNDED NORMAL NONET, "ø̞̈"),(Vowel MID CENTRAL UNROUNDED NORMAL NONET, "ə"),(Vowel MID CENTRAL ROUNDED NORMAL NONET, "ɵ̞"),(Vowel MID NEARBACK UNROUNDED NORMAL NONET, "ɤ̞̈"),(Vowel MID NEARBACK ROUNDED NORMAL NONET, "ö̞"),(Vowel MID BACK UNROUNDED NORMAL NONET, "ɤ̞"),(Vowel MID BACK ROUNDED NORMAL NONET, "o̞")]
    , [(Vowel OPENMID FRONT UNROUNDED NORMAL NONET, "ɛ"),(Vowel OPENMID FRONT ROUNDED NORMAL NONET, "œ"),(Vowel OPENMID NEARFRONT UNROUNDED NORMAL NONET, "ɛ̈"),(Vowel OPENMID NEARFRONT ROUNDED NORMAL NONET, "œ̈"),(Vowel OPENMID CENTRAL UNROUNDED NORMAL NONET, "ɜ"),(Vowel OPENMID CENTRAL ROUNDED NORMAL NONET, "ɞ"),(Vowel OPENMID NEARBACK UNROUNDED NORMAL NONET, "ʌ̈"),(Vowel OPENMID NEARBACK ROUNDED NORMAL NONET, "ɔ̈"),(Vowel OPENMID BACK UNROUNDED NORMAL NONET, "ʌ"),(Vowel OPENMID BACK ROUNDED NORMAL NONET, "ɔ")]
    , [(Vowel NEAROPEN FRONT UNROUNDED NORMAL NONET, "æ"),(Vowel NEAROPEN FRONT ROUNDED NORMAL NONET, "œ̞"),(Vowel NEAROPEN NEARFRONT UNROUNDED NORMAL NONET, "a̽"),(Vowel NEAROPEN NEARFRONT ROUNDED NORMAL NONET, "ɶ̽"),(Vowel NEAROPEN CENTRAL UNROUNDED NORMAL NONET, "ɐ"),(Vowel NEAROPEN CENTRAL ROUNDED NORMAL NONET, "ɞ̞"),(Vowel NEAROPEN NEARBACK UNROUNDED NORMAL NONET, "ɑ̽"),(Vowel NEAROPEN NEARBACK ROUNDED NORMAL NONET, "ɒ̽"),(Vowel NEAROPEN BACK UNROUNDED NORMAL NONET, "ʌ̞"),(Vowel NEAROPEN BACK ROUNDED NORMAL NONET, "ɔ̞")]
    , [(Vowel OPEN FRONT UNROUNDED NORMAL NONET, "a"),(Vowel OPEN FRONT ROUNDED NORMAL NONET, "ɶ"),(Vowel OPEN NEARFRONT UNROUNDED NORMAL NONET, "a̠"),(Vowel OPEN NEARFRONT ROUNDED NORMAL NONET, "ɶ̠"),(Vowel OPEN CENTRAL UNROUNDED NORMAL NONET, "ä"),(Vowel OPEN CENTRAL ROUNDED NORMAL NONET, "ɒ̈"),(Vowel OPEN NEARBACK UNROUNDED NORMAL NONET, "ɑ̟"),(Vowel OPEN NEARBACK ROUNDED NORMAL NONET, "ɒ̟"),(Vowel OPEN BACK UNROUNDED NORMAL NONET, "ɑ"),(Vowel OPEN BACK ROUNDED NORMAL NONET, "ɒ")]
    ]
