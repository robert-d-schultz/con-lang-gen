module EnglishStuff
( englishGrammar
, englishInflection
, englishVerbInfl
) where

import GrammarData
import InflectionData

englishGrammar :: Grammar
englishGrammar = Grammar SubInitial ObjFinal CompFinal OblVtoIMove NoAffixHop NoNullSub OptTopic NoNullTop NoTopMark OblItoCMove OblWHMove PiedPipe OblQuesInv

englishInflection :: InflectionSystem
englishInflection = InflectionSystem
                      { genSys = Manifest [(Pron, Affix, 1)] [M, F, N]
                      , aniSys = Manifest [(Pron, Affix, 1)] [AN, INAN]
                      , casSys = Manifest [(Pron, Affix, 1), (Noun, Affix, 1)] [NOM, ACC, GEN]
                      , numSys = Manifest [(Verb, Affix, 1), (Noun, Affix, 1), (Noun, Particle, 1)] [SG, PL]
                      , defSys = Manifest [(Noun, Particle, 1)] [DEF, INDF]
                      , speSys = NoManifest
                      , topSys = NoManifest
                      , perSys = Manifest [(Pron, Affix, 1), (Noun, Affix, 1)] [FIRST, SECOND, THIRD]
                      , honSys = NoManifest
                      , polSys = Manifest [(Verb, Particle, 1)] [AFF, NEG]
                      , tenSys = Manifest [(Verb, Affix, 1)] [PST, PRS, FUT]
                      , aspSys = Manifest [(Verb, Particle, 2), (Verb, Affix, 1)] [PFV, IPFV]
                      , mooSys = Manifest [(Verb, Particle, 2), (Verb, Affix, 1)] [IND, SBJV, COND, IMP]
                      , voiSys = Manifest [(Verb, Affix, 1)] [ACTIVE, PASSIVE]
                      , eviSys = NoManifest
                      , traSys = NoManifest
                      , volSys = Manifest [(Verb, Particle, 3)] [VOL, NVOL]
                      }

-- englishInflections :: [ManifestSystem]
englishInflections = [
    -- Pron Affix 1
    [ ( "he", ( Express M, Express AN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "him", ( Express M, Express AN, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "his", ( Express M, Express AN, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "she", ( Express F, Express AN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "her", ( Express F, Express AN, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "her", ( Express F, Express AN, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "they", ( Express N, Express AN, Express NOM, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "them", ( Express N, Express AN, Express ACC, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "their", ( Express N, Express AN, Express GEN, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "they", ( Express N, Express AN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "them", ( Express N, Express AN, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "their", ( Express N, Express AN, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "it", ( Express N, Express INAN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "it", ( Express N, Express INAN, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "its", ( Express N, Express INAN, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "they", ( Express N, Express INAN, Express NOM, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "them", ( Express N, Express INAN, Express ACC, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "their", ( Express N, Express INAN, Express GEN, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "you", ( Express N, Express AN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "you", ( Express N, Express AN, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "your", ( Express N, Express AN, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "you", ( Express N, Express AN, Express NOM, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "you", ( Express N, Express AN, Express ACC, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "your", ( Express N, Express AN, Express GEN, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "I", ( Express N, Express AN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "me", ( Express N, Express AN, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "my", ( Express N, Express AN, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "we", ( Express N, Express AN, Express NOM, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "us", ( Express N, Express AN, Express ACC, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "our", ( Express N, Express AN, Express GEN, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    ]

  --Noun particle 1
  , [ ( "the", ( NoExpress, NoExpress, NoExpress, Express SG, Express DEF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "the", ( NoExpress, NoExpress, NoExpress, Express PL, Express DEF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "a", ( NoExpress, NoExpress, NoExpress, Express SG, Express INDF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, Express INDF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    ]

  --Noun affix 1
  , [ ( "s", ( NoExpress, NoExpress, Express NOM, Express PL, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "s", ( NoExpress, NoExpress, Express ACC, Express PL, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "s\'", ( NoExpress, NoExpress, Express GEN, Express PL, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "\'s", ( NoExpress, NoExpress, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    ]

  --Verb particle 1 (negation)
  , [ ( "not", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express NEG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express AFF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    ]

  --Verb particle 2
  , [ ( "will", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express FUT, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express PRS, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express PST, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
  ]

  --Verb particle 3 (active/passive voice)
  , [ ( "by", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express PASSIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    ]
  ]
englishVerbInfl :: [(String, (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition))]
englishVerbInfl =
  --Verb affix 1
   [ ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "s", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express IPFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express PFV, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express IPFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express PFV, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
    ]
