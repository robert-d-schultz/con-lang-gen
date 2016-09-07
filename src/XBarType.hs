module XBarType
( Optional(..)
, NounP(..)
, NounBar(..)
, Noun(..)
, VerbP(..)
, VerbBar(..)
, Verb(..)
, AdjP(..)
, AdjBar(..)
, Adj(..)
, AdvP(..)
, AdvBar(..)
, Adv(..)
, PrepP(..)
, PrepBar(..)
, Prep(..)
, DetP(..)
, DetBar(..)
, Det(..)
, CompP(..)
, CompBar(..)
, Comp(..)
, TenseP(..)
, TenseBar(..)
, Tense(..)
) where

-- X-bar theory data

--data Reverse a b = XbarFirst a b | XbarSecond b a

data Optional a = YesOpt a | NoOpt  deriving (Show, Read)

--Noun phrase
data NounP = NounP NounBar deriving (Show, Read)
data NounBar = NounBar1 AdjP NounBar | NounBar2 NounBar PrepP | NounBar3 Noun (Optional PrepP) deriving (Show, Read)
data Noun = Noun String deriving (Show, Read)

--Verb phrase
data VerbP = VerbP VerbBar deriving (Show, Read)
data VerbBar = VerbBar1 AdvP VerbBar | VerbBar2 VerbBar PrepP | VerbBar3 VerbBar AdvP | VerbBar4 Verb CompP | VerbBar5 Verb NounP deriving (Show, Read)
data Verb = Verb String deriving (Show, Read)

--Adjective phrase
data AdjP = AdjP AdjBar deriving (Show, Read)
data AdjBar = AdjBar1 AdvP AdjBar | AdjBar2 AdjP AdjBar | AdjBar3 AdjBar (Optional PrepP) | AdjBar4 Adj (Optional PrepP) deriving (Show, Read)
data Adj = Adj String deriving (Show, Read)

--Adverb phrase
data AdvP = AdvP (Optional AdvP) AdvBar deriving (Show, Read)
data AdvBar = AdvBar Adv deriving (Show, Read)
data Adv = Adv String deriving (Show, Read)

--Prepositional phrase
data PrepP = PrepP (Optional AdjP) PrepBar deriving (Show, Read)
data PrepBar = PrepBar1 PrepBar (Optional PrepP) | PrepBar2 Prep DetP deriving (Show, Read)
data Prep = Prep String deriving (Show, Read)

--Determiner phrase
data DetP = DetP DetBar deriving (Show, Read)
data DetBar = DetBar (Optional Det) NounP deriving (Show, Read)
data Det = Det String deriving (Show, Read)

--Complementizer phrase
data CompP = CompP CompBar deriving (Show, Read)
data CompBar = CompBar Comp TenseP deriving (Show, Read)
data Comp = Comp String deriving (Show, Read)

--Tense phrase
data TenseP = TenseP DetP TenseBar deriving (Show, Read)
data TenseBar = TenseBar Tense VerbP deriving (Show, Read)
data Tense = Tense String deriving (Show, Read)

--Inflection phrase
--data InflP = InfP InflBar DetP
--data InflBar = InflBar Either InflBar Adjunct Infl Comp
--data Infl = Infl String
