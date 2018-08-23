{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module EnglishStuff
( englishLanguage
, englishGrammar
, englishInflection
, englishManifest
) where

import ClassyPrelude

import Data.Grammar
import Data.Inflection
import Data.Other

englishLanguage = Language { getName = "English"
                           , getCMap = ([], [], [])
                           , getCInv = []
                           , getVMap = ([], [], [], [], [])
                           , getVInv = []
                           , getDInv = []
                           , getSonHier = 0
                           , getOnsetCCs = []
                           , getNuclei = []
                           , getCodaCCs = []
                           , getInflMap = englishInflection
                           , getManSyss = []
                           , getGrammar = englishGrammar
                           , getRoots = []
                           , getWriting = ([], [], [])
                           }

englishGrammar :: Grammar
englishGrammar = Grammar SubInitial ObjFinal CompFinal NoVtoIMove OblAffixHop NoNullSub OptTopic NoNullTop NoTopMark OblItoCMove OblWHMove PiedPipe OblQuesInv

englishInflection :: InflectionMap
englishInflection = InflectionMap
                      { getGenSys = Manifest [(Pron, Suffix, 1)] [M, F, N]
                      , getAniSys = Manifest [(Pron, Suffix, 1)] [AN, INAN]
                      , getCasSys = Manifest [(Pron, Suffix, 1), (Noun, Suffix, 1)] [NOM, ACC, GEN]
                      , getNumSys = Manifest [(Verb, Suffix, 1), (Noun, Suffix, 1), (Noun, Particle, 1)] [SG, PL]
                      , getDefSys = Manifest [(Noun, Particle, 1)] [DEF, INDF]
                      , getSpeSys = Manifest [(Noun, Particle, 2)] [SPEC, NSPEC] -- "fake"
                      , getTopSys = Manifest [(Noun, Suffix, 2)] [TOP, NTOP] --"fake"
                      , getPerSys = Manifest [(Pron, Suffix, 1), (Noun, Suffix, 1)] [FIRST, SECOND, THIRD]
                      , getHonSys = NoManifest
                      , getPolSys = Manifest [(Verb, Particle, 1)] [AFF, NEG]
                      , getTenSys = Manifest [(Verb, Suffix, 1)] [PST, PRS, FUT, PSTPER, PRSPER, FUTPER]
                      , getAspSys = Manifest [(Verb, Particle, 2), (Verb, Suffix, 1)] [NNPROG, PROG]
                      , getMooSys = Manifest [(Verb, Particle, 2), (Verb, Suffix, 1)] [IND, SBJV, COND, IMP]
                      , getVoiSys = Manifest [(Verb, Particle, 1), (Verb, Suffix, 1)] [ACTIVE, PASSIVE]
                      , getEviSys = NoManifest
                      , getTraSys = NoManifest
                      , getVolSys = Manifest [(Verb, Particle, 3)] [VOL, NVOL] --"fake"
                      }

englishManifest :: [(LexCat, [(LexCat, ManifestType, [(Text, AllExpress)])], [(LexCat, ManifestType, [(Text, AllExpress)])], [(LexCat, ManifestType, [(Text, AllExpress)])])]
englishManifest = [eVerbInfl, eNounInfl, ePronInfl]

eVerbInfl :: (LexCat, [(LexCat, ManifestType, [(Text, AllExpress)])], [(LexCat, ManifestType, [(Text, AllExpress)])], [(LexCat, ManifestType, [(Text, AllExpress)])])
eVerbInfl = (Verb, [eVerbParticle1, eVerbParticle2],[],[eVerbSuffix1])

eNounInfl = (Noun, [eNounParticle1],[],[eNounSuffix1])
ePronInfl = (Pron, [ePronParticle1],[],[])

ePronParticle1 :: (LexCat, ManifestType, [(Text, AllExpress)])
ePronParticle1 = (Pron, Particle, [ ( "he", ( Express M, Express AN, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
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
                                  ])

eVerbParticle2 = (Verb, Particle, [ ( "not", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express NEG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express AFF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  ])
eVerbParticle3 = (Verb, Particle, [ ( "(on purpose)", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express VOL ) )
                                  , ( "(accidentally)", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express NVOL ) )
                                  ])

eNounParticle1 = (Noun, Particle, [ ( "the", ( NoExpress, NoExpress, NoExpress, Express SG, Express DEF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  , ( "the", ( NoExpress, NoExpress, NoExpress, Express PL, Express DEF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  , ( "a", ( NoExpress, NoExpress, NoExpress, Express SG, Express INDF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, Express INDF, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  ])

eNounParticle2 = (Noun, Particle, [ ( "(specific)", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express SPEC, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  , ( "(nonspecific)", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express NSPEC, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                                  ])

eNounSuffix1 = (Noun, Suffix, [ ( "s", ( NoExpress, NoExpress, Express NOM, Express PL, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, Express NOM, Express SG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              , ( "s", ( NoExpress, NoExpress, Express ACC, Express PL, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, Express ACC, Express SG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              , ( "s\'", ( NoExpress, NoExpress, Express GEN, Express PL, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              , ( "\'s", ( NoExpress, NoExpress, Express GEN, Express SG, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              ])

eNounSuffix2 = (Noun, Suffix, [ ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express TOP, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, Express NTOP, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress, NoExpress ) )
                              ])

eVerbParticle1 = (Verb, Particle, [ ( "will", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "has", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "will have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "have", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  , ( "had", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                                  ])

eVerbSuffix1 =(Verb, Suffix, [ ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "s", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express IND, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PST, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRS, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUT, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express SG, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express THIRD, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express SECOND, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PSTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express PRSPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              , ( "ed", ( NoExpress, NoExpress, NoExpress, Express PL, NoExpress, NoExpress, NoExpress, Express FIRST, NoExpress, NoExpress, Express FUTPER, Express NNPROG, Express SBJV, Express ACTIVE, NoExpress, NoExpress, NoExpress ) )
                              ])
