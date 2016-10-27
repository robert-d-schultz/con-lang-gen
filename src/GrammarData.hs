module GrammarData
(Grammar(..)
) where

-- Language grammar
data Grammar = Grammar
    { si   :: SubjectInitial
      of   :: ObjectFinal
      ci   :: ComplementizerInitial
      obt  :: ObligatoryTopic
      ns   :: NullSubject
      nt   :: NullTopic
      whm  :: WHMovementObligatory
      pi   :: PiedPiping
      tm   :: TopicMarking
      vtol :: VtoIMovement
      itoc :: ItoCMovement
      ah   :: AffixHopping
      qinv :: QuestionInversion
    } deriving (Show)

-- Grammar parameters
data SubjectInitial        = SubInitial | SubFinal
data ObjectFinal           = ObjFinal | ObjInitial
data ComplementizerInitial = CompInitial | CompFinal
data ObligatoryTopic       = OblTopic | OptTopic
data NullSubject           = NoNullSub | OptNullSub
data NullTopic             = NoNullTop | OptNullTop
data WHMovementObligatory  = NoWHMove | OblWHMove
data PiedPiping            = PiedPipe | PrepStrand
data TopicMarking          = NoTopMark | OblTopMark
data VtoIMovement          = NoVtoIMove | OblVtoIMove
data ItoCMovement          = NoItoCMove | OblItoCMove
data AffixHopping          = NoAffixHop | OblAffixHop
data QuestionInversion     = NoQuesInv | OblQuesInv


-- Parse tree stuff
-- CP
-- CBar
-- IP
-- IBar AFFIRM?
-- IBar NEG?
-- NegP
-- NegBar
-- VP
-- VBar
-- PP
-- PBar
-- Hm. They didn't use DP or AdjP or anything fancy
