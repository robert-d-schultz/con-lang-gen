module Out.Grammar
( parseGrammar
) where

import Data.List

import Data.Grammar

-- Parse grammar system
parseGrammar :: Grammar -> String
parseGrammar (Grammar si ob ci vi ah ns ot nt tm ic wm pp qi) = "\nParameters:" ++ out where
  out = "\n\t*" ++ intercalate "\n\t*" [ parseSubjectInitial si
                                  , parseObjectFinal ob
                                  , parseComplementizerInitial ci
                                  , parseVtoIMovement vi
                                  , parseAffixHopping ah
                                  , parseNullSubject ns
                                  , parseObligatoryTopic ot
                                  , parseNullTopic nt
                                  , parseTopicMarking tm
                                  , parseItoCMovement ic
                                  , parseWHMovementObligatory wm
                                  , parsePiedPiping pp
                                  , parseQuestionInversion qi
                                  ]

parseSubjectInitial :: SubjectInitial -> String
parseSubjectInitial si
  | si == SubInitial = "Specifier initial"
  | si == SubFinal = "Specifier final"
parseObjectFinal :: ObjectFinal -> String
parseObjectFinal ob
  | ob == ObjFinal = "Object final"
  | ob == ObjInitial = "Object initial"
parseComplementizerInitial :: ComplementizerInitial -> String
parseComplementizerInitial pc
  | pc == CompInitial = "(Non-object) Head initial"
  | pc == CompFinal = "(Non-object) Head final"
parseVtoIMovement :: VtoIMovement -> String
parseVtoIMovement vi
  | vi == NoVtoIMove = "No V to I movement"
  | vi == OblVtoIMove = "Obligatory V to I movement"
parseAffixHopping :: AffixHopping -> String
parseAffixHopping ah
  | ah == NoAffixHop = "No affix hoping"
  | ah == OblAffixHop = "Obligatory affix hoping"
parseNullSubject :: NullSubject -> String
parseNullSubject ns
  | ns == NoNullSub = "No null subject"
  | ns == OptNullSub = "Optional null subject"
parseObligatoryTopic :: ObligatoryTopic -> String
parseObligatoryTopic ot
  | ot == OblTopic = "Obligatory topic"
  | ot == OptTopic = "Optional topic"
parseNullTopic :: NullTopic -> String
parseNullTopic nt
  | nt == NoNullTop = "No null topic"
  | nt == OptNullTop = "Optional null topic"
parseTopicMarking :: TopicMarking -> String
parseTopicMarking tm
  | tm == NoTopMark = "No topic marking"
  | tm == OblTopMark = "Obligatory topic marking"
parseItoCMovement :: ItoCMovement -> String
parseItoCMovement ic
  | ic == NoItoCMove = "No I to C movement"
  | ic == OblItoCMove = "Obligatory I to C movement"
parseWHMovementObligatory :: WHMovementObligatory -> String
parseWHMovementObligatory wm
  | wm == NoWHMove = "No WH-movement"
  | wm == OblWHMove = "Obligatory WH-movement"
parsePiedPiping :: PiedPiping -> String
parsePiedPiping pp
  | pp == PiedPipe = "Piedpiping"
  | pp == PrepStrand = "Preposition stranding"
parseQuestionInversion :: QuestionInversion -> String
parseQuestionInversion qi
  | qi == NoQuesInv = "No question inversion"
  | qi == OblQuesInv = "Obligatory question inversion"
