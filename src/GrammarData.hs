module GrammarData
( Grammar(..)
, SubjectInitial(..)
, ObjectFinal(..)
, ComplementizerInitial(..)
, ObligatoryTopic(..)
, NullSubject(..)
, NullTopic(..)
, WHMovementObligatory(..)
, PiedPiping(..)
, TopicMarking(..)
, VtoIMovement(..)
, ItoCMovement(..)
, AffixHopping(..)
, QuestionInversion(..)
, Illoc(..)
, LeafType(..)
, Phrase(..)
, Bar(..)
, Leaf(..)
) where

import InflectionData

-- Language grammar
data Grammar = Grammar
            { getSI   :: SubjectInitial
            , getOF   :: ObjectFinal
            , getCI   :: ComplementizerInitial
            , getVtoI :: VtoIMovement
            , getAH   :: AffixHopping
            , getNS   :: NullSubject
            , getObT  :: ObligatoryTopic
            , getNT   :: NullTopic
            , getTM   :: TopicMarking
            , getItoC :: ItoCMovement
            , getWHM  :: WHMovementObligatory
            , getPP   :: PiedPiping
            , getQI   :: QuestionInversion
            } deriving (Show)

-- Grammar parameters
data SubjectInitial        = SubInitial  | SubFinal    deriving (Eq, Enum, Show, Read)
data ObjectFinal           = ObjFinal    | ObjInitial  deriving (Eq, Enum, Show, Read)
data ComplementizerInitial = CompInitial | CompFinal   deriving (Eq, Enum, Show, Read)
data VtoIMovement          = NoVtoIMove  | OblVtoIMove deriving (Eq, Enum, Show, Read)
data AffixHopping          = NoAffixHop  | OblAffixHop deriving (Eq, Enum, Show, Read)
data NullSubject           = NoNullSub   | OptNullSub  deriving (Eq, Enum, Show, Read)
data ObligatoryTopic       = OblTopic    | OptTopic    deriving (Eq, Enum, Show, Read)
data NullTopic             = NoNullTop   | OptNullTop  deriving (Eq, Enum, Show, Read)
data TopicMarking          = NoTopMark   | OblTopMark  deriving (Eq, Enum, Show, Read)
data ItoCMovement          = NoItoCMove  | OblItoCMove deriving (Eq, Enum, Show, Read)
data WHMovementObligatory  = NoWHMove    | OblWHMove   deriving (Eq, Enum, Show, Read)
data PiedPiping            = PiedPipe    | PrepStrand  deriving (Eq, Enum, Show, Read)
data QuestionInversion     = NoQuesInv   | OblQuesInv  deriving (Eq, Enum, Show, Read)

-- Parse tree, keep it simple stupid
data Illoc = Null | Ques | Decl | Imper deriving (Eq, Enum, Show)

data LeafType = LeafWord | LeafAffix deriving (Eq, Enum, Show)

data Phrase = XPNull
            | XP
                { phraseLC :: LexCat
                , phraseIl :: Illoc
                , specifier :: Phrase
                , phraseBar :: Bar
                } deriving (Eq, Show)

data Bar = XBarA
             { barLC  :: LexCat
             , barIl :: Illoc
             , adjunct :: Phrase
             , barBar :: Bar
             }
         | XBarC
             { barLC :: LexCat
             , barIl :: Illoc
             , leaf :: Leaf
             , compliment :: Phrase
             }  deriving (Eq, Show)

data Leaf = LeafNull Illoc
          | LeafInfl
            { leafInfl :: (Express Gender, Express Animacy, Express Case, Express Number, Express Definiteness, Express Specificity, Express Topic, Express Person, Express Honorific, Express Polarity, Express Tense, Express Aspect, Express Mood, Express Voice, Express Evidentiality, Express Transitivity, Express Volition)
            }
          | Leaf
            { leafLC  :: LexCat
            , leafIl  :: Illoc
            , leafT   :: LeafType
            , leafStr :: String
            } deriving (Eq, Show)
