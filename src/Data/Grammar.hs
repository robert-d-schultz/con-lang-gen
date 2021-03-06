{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Grammar
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
, Phrase(..)
, Bar(..)
, Leaf(..)
, showtree
) where

import ClassyPrelude
import Data.Text

import Data.Inflection
import Data.Word

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
data SubjectInitial        = SubInitial  | SubFinal    deriving (Eq, Enum, Bounded)
data ObjectFinal           = ObjFinal    | ObjInitial  deriving (Eq, Enum, Bounded)
data ComplementizerInitial = CompInitial | CompFinal   deriving (Eq, Enum, Bounded)
data VtoIMovement          = NoVtoIMove  | OblVtoIMove deriving (Eq, Enum, Bounded)
data AffixHopping          = NoAffixHop  | OblAffixHop deriving (Eq, Enum, Bounded)
data NullSubject           = NoNullSub   | OptNullSub  deriving (Eq, Enum, Bounded)
data ObligatoryTopic       = OblTopic    | OptTopic    deriving (Eq, Enum, Bounded)
data NullTopic             = NoNullTop   | OptNullTop  deriving (Eq, Enum, Bounded)
data TopicMarking          = NoTopMark   | OblTopMark  deriving (Eq, Enum, Bounded)
data ItoCMovement          = NoItoCMove  | OblItoCMove deriving (Eq, Enum, Bounded)
data WHMovementObligatory  = NoWHMove    | OblWHMove   deriving (Eq, Enum, Bounded)
data PiedPiping            = PiedPipe    | PrepStrand  deriving (Eq, Enum, Bounded)
data QuestionInversion     = NoQuesInv   | OblQuesInv  deriving (Eq, Enum, Bounded)

-- show instances
instance Show SubjectInitial where
  show si = case si of SubInitial -> "Specifier initial"
                       SubFinal -> "Specifier final"

instance Show ObjectFinal where
  show ob = case ob of ObjFinal -> "Object final"
                       ObjInitial -> "Object initial"

instance Show ComplementizerInitial where
  show pc = case pc of CompInitial -> "(Non-object) Head initial"
                       CompFinal -> "(Non-object) Head final"

instance Show VtoIMovement where
  show vi = case vi of NoVtoIMove -> "No V to I movement"
                       OblVtoIMove -> "Obligatory V to I movement"

instance Show AffixHopping where
  show ah = case ah of NoAffixHop -> "No affix hoping"
                       OblAffixHop -> "Obligatory affix hoping"

instance Show NullSubject where
  show ns = case ns of NoNullSub -> "No null subject"
                       OptNullSub -> "Optional null subject"

instance Show ObligatoryTopic where
  show ot = case ot of OblTopic -> "Obligatory topic"
                       OptTopic -> "Optional topic"

instance Show NullTopic where
  show nt = case nt of NoNullTop -> "No null topic"
                       OptNullTop -> "Optional null topic"

instance Show TopicMarking where
  show tm = case tm of NoTopMark -> "No topic marking"
                       OblTopMark -> "Obligatory topic marking"

instance Show ItoCMovement where
  show ic = case ic of NoItoCMove -> "No I to C movement"
                       OblItoCMove -> "Obligatory I to C movement"

instance Show WHMovementObligatory where
  show wm = case wm of NoWHMove -> "No WH-movement"
                       OblWHMove -> "Obligatory WH-movement"

instance Show PiedPiping where
  show pp = case pp of PiedPipe -> "Piedpiping"
                       PrepStrand -> "Preposition stranding"

instance Show QuestionInversion where
  show qi = case qi of NoQuesInv -> "No question inversion"
                       OblQuesInv -> "Obligatory question inversion"


-- Parse tree, keep it simple stupid
data Illoc = Null | Ques | Decl | Imper deriving (Eq, Enum, Show)

data Phrase = XPNull
            | XP
            { phraseLC :: LexCat
            , phraseIl :: Illoc
            , specifier :: Phrase
            , phraseBar :: Bar
            } deriving (Eq, Show)

data Bar = XBarA
         { barLC  :: LexCat -- ?
         , barIl :: Illoc
         , adjunct :: Phrase
         , barBar :: Bar
         }
         | XBarC
         { barLC :: LexCat -- ?
         , barIl :: Illoc
         , leaf :: Leaf
         , compliment :: Phrase
         }  deriving (Eq, Show)

data Leaf = LeafNull Illoc
          | LeafInfl
          { leafLC :: LexCat -- ?
          , leafMorphs :: [Morpheme]
          }
          | Leaf
          { leafLC  :: LexCat -- ?
          , leafIl  :: Illoc -- fine
          , leafMorph:: Morpheme -- ?
          } deriving (Eq, Show)

class ShowTree a where
  showtree :: a -> Text
  showtree_ :: Text -> Text -> a -> Text

instance ShowTree Phrase where
  showtree x = replace " " "&nbsp;" (showtree_ " " " " x)
  showtree_ pad char XPNull = ""
  showtree_ pad char (XP lc il XPNull bar) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "P" ++ showtree_ (pad++" ") "└" bar
  showtree_ pad char (XP lc il spec bar) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "P" ++ showtree_ (pad++"|") "├" spec ++ showtree_ (pad++" ") "└" bar

instance ShowTree Bar where
  showtree = showtree_ " " " "
  showtree_ pad char (XBarA lc il adj bar) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "Bar" ++ showtree_ (pad++"|") "├" adj ++ showtree_ (pad++" ") "└" bar
  showtree_ pad char (XBarC lc il leaf XPNull) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "Bar" ++ showtree_ (pad++" ") "└" leaf
  showtree_ pad char (XBarC lc il leaf phrase) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "Bar" ++ showtree_ (pad++"|") "├" leaf ++ showtree_ (pad++" ") "└" phrase

instance ShowTree Leaf where
  showtree = showtree_ " " " "
  showtree_ pad char (LeafNull il) = "\n<br>\n" ++ unsafeInit pad ++ char ++ "null"
  showtree_ pad char (LeafInfl lc morphs) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "\n<br>\n" ++ pad ++ "└" ++ tshow morphs
  showtree_ pad char (Leaf lc il morph) = "\n<br>\n" ++ unsafeInit pad ++ char ++ tshow lc ++ "\n<br>\n" ++ pad ++ "└\"" ++ tshow morph ++ "\""
