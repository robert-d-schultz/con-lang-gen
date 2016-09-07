module XBar
( makeTenseP
, parseTenseP
, loadInputData
) where

import           Control.Monad
import           Data.Random.Extras hiding (shuffle)
import           Data.RVar
import           Prelude
import           XBarType

-- Load words from files
data InputData = InputData
    {
      iNoun :: [Noun]
    , iVerb :: [Verb]
    , iAdj  :: [Adj]
    , iAdv  :: [Adv]
    , iPrep :: [Prep]
    , iDet  :: [Det]
    , iComp :: [Comp]
    }

loadInputData :: IO InputData
loadInputData  =
    InputData
        <$> readFeature "raw/pos/nouns.txt"
        <*> readFeature "raw/pos/verbs.txt"
        <*> readFeature "raw/pos/adjectives.txt"
        <*> readFeature "raw/pos/adverbs.txt"
        <*> readFeature "raw/pos/prepositions.txt"
        <*> readFeature "raw/pos/determiners.txt"
        <*> readFeature "raw/pos/complementizers.txt"

readFeature :: Read a => FilePath -> IO a
readFeature = fmap read . readFile

--Make random syntax tree

makeNoun :: InputData -> RVar Noun
makeNoun idata =
  choice (iNoun idata)

makeVerb :: InputData -> RVar Verb
makeVerb idata =
  choice (iVerb idata)

makeAdj :: InputData -> RVar Adj
makeAdj idata =
  choice (iAdj idata)

makeAdv :: InputData -> RVar Adv
makeAdv idata =
  choice (iAdv idata)

makePrep :: InputData -> RVar Prep
makePrep idata =
  choice (iPrep idata)

makeDet :: InputData -> RVar Det
makeDet idata =
  choice (iDet idata)

makeComp :: InputData -> RVar Comp
makeComp idata =
  choice (iComp idata)

makeTense :: InputData -> RVar Tense
makeTense idata =
  choice [Tense "-ed"]

--NounBar main
makeNounBar :: InputData -> Int  -> RVar NounBar
makeNounBar idata n
  | n <= 0    = join $ choice [makeNounBar32 idata 0]
  | otherwise = join $ choice [makeNounBar1 idata i, makeNounBar2 idata i, makeNounBar31 idata i, makeNounBar32 idata i]
  where i = n-1

--AdjP + NounBar
makeNounBar1 :: InputData -> Int  -> RVar NounBar
makeNounBar1 idata n =
  NounBar1 <$> (makeAdjP idata i) <*> (makeNounBar idata i)
  where i = n-1

--NounBar + PrepP
makeNounBar2 :: InputData -> Int  -> RVar NounBar
makeNounBar2 idata n =
  NounBar2 <$> (makeNounBar idata i) <*> (makePrepP idata i)
  where i = n-1

--Noun with optional PrepP
makeNounBar31 :: InputData -> Int  -> RVar NounBar
makeNounBar31 idata n =
  NounBar3 <$> (makeNoun idata) <*> (YesOpt <$> (makePrepP idata i))
  where i = n-1

--Noun without optional PrepP
makeNounBar32 :: InputData -> Int  -> RVar NounBar
makeNounBar32 idata n = do
  let i = n-1
  foo <- (makeNoun idata)
  return (NounBar3 foo (NoOpt))

--VerbBar main
makeVerbBar :: InputData -> Int  -> RVar VerbBar
makeVerbBar idata n
  | n <= 0    = join $ choice [makeVerbBar4 idata 0, makeVerbBar5 idata 0]
  | otherwise = join $ choice [makeVerbBar1 idata i, makeVerbBar2 idata i, makeVerbBar3 idata i, makeVerbBar4 idata i, makeVerbBar5 idata i]
  where i = n-1

--AdvP + VerbBar
makeVerbBar1 :: InputData -> Int  -> RVar VerbBar
makeVerbBar1 idata n =
  VerbBar1 <$> (makeAdvP idata i) <*> (makeVerbBar idata i)
  where i = n-1

--VerbBar + PrepP
makeVerbBar2 :: InputData -> Int  -> RVar VerbBar
makeVerbBar2 idata n =
  VerbBar2 <$> (makeVerbBar idata i) <*> (makePrepP idata i)
  where i = n-1

--VerbBar + AdvP
makeVerbBar3 :: InputData -> Int  -> RVar VerbBar
makeVerbBar3 idata n =
  VerbBar3 <$> (makeVerbBar idata i) <*> (makeAdvP idata i)
  where i = n-1

--Verb + CompP
makeVerbBar4 :: InputData -> Int  -> RVar VerbBar
makeVerbBar4 idata n =
  VerbBar4 <$> (makeVerb idata) <*> (makeCompP idata i)
  where i = n-1

--Verb + NounP
makeVerbBar5 :: InputData -> Int  -> RVar VerbBar
makeVerbBar5 idata n =
  VerbBar5 <$> (makeVerb idata) <*> (makeNounP idata i)
  where i = n-1

--AdjBar main
makeAdjBar :: InputData -> Int  -> RVar AdjBar
makeAdjBar idata n
  | n <= 0    = join $ choice [makeAdjBar42 idata 0]
  | otherwise = join $ choice [makeAdjBar1 idata i, makeAdjBar2 idata i, makeAdjBar31 idata i, makeAdjBar32 idata i, makeAdjBar41 idata i, makeAdjBar42 idata i]
  where i = n-1

--AdvP + AdjBar
makeAdjBar1 :: InputData -> Int  -> RVar AdjBar
makeAdjBar1 idata n =
  AdjBar1 <$> (makeAdvP idata i) <*> (makeAdjBar idata i)
  where i = n-1

--AdjP + AdjBar
makeAdjBar2 :: InputData -> Int  -> RVar AdjBar
makeAdjBar2 idata n =
  AdjBar2 <$> (makeAdjP idata i) <*> (makeAdjBar idata i)
  where i = n-1

--AdjBar with the optional PrepP
makeAdjBar31 :: InputData -> Int  -> RVar AdjBar
makeAdjBar31 idata n =
  AdjBar3 <$> (makeAdjBar idata i) <*> (YesOpt <$> (makePrepP idata i))
  where i = n-1

--AdjBar without the optional PrepP
makeAdjBar32 :: InputData -> Int  -> RVar AdjBar
makeAdjBar32 idata n = do
  let i = n-1
  foo <- (makeAdjBar idata i)
  return (AdjBar3 foo (NoOpt))

--Adj with the optional PrepP
makeAdjBar41 :: InputData -> Int  -> RVar AdjBar
makeAdjBar41 idata n =
  AdjBar4 <$> (makeAdj idata) <*> (YesOpt <$> (makePrepP idata i))
  where i = n-1

--Adj without the optional PrepP
makeAdjBar42 :: InputData -> Int  -> RVar AdjBar
makeAdjBar42 idata n = do
  let i = n-1
  foo <- (makeAdj idata)
  return (AdjBar4 foo (NoOpt))

--AdvBar
makeAdvBar :: InputData -> Int  -> RVar AdvBar
makeAdvBar idata n =
  AdvBar <$> (makeAdv idata)
  where i = n-1

--PrepBar main
makePrepBar :: InputData -> Int  -> RVar PrepBar
makePrepBar idata n
  | n <= 0    = join $ choice [makePrepBar2 idata 0]
  | otherwise = join $ choice [makePrepBar11 idata i, makePrepBar12 idata i, makePrepBar2 idata i]
  where i = n-1

--PrepBar + with the optional PrepP
makePrepBar11 :: InputData -> Int  -> RVar PrepBar
makePrepBar11 idata n =
  PrepBar1 <$> (makePrepBar idata i) <*> (YesOpt <$> (makePrepP idata i))
  where i = n-1

--PrepBar + without the optional PrepP
makePrepBar12 :: InputData -> Int  -> RVar PrepBar
makePrepBar12 idata n = do
  let i = n-1
  foo <- (makePrepBar idata i)
  return (PrepBar1 foo (NoOpt))

--Prep + DetP
makePrepBar2 :: InputData -> Int  -> RVar PrepBar
makePrepBar2 idata n =
  PrepBar2 <$> (makePrep idata) <*> (makeDetP idata i)
  where i = n-1

--DetBar main
makeDetBar :: InputData -> Int  -> RVar DetBar
makeDetBar idata n =
  join $ choice [makeDetBar1 idata i, makeDetBar2 idata i]
  where i = n-1

--with optional determiner
makeDetBar1 :: InputData -> Int  -> RVar DetBar
makeDetBar1 idata n =
  DetBar <$> (YesOpt <$> (makeDet idata)) <*> (makeNounP idata i)
  where i = n-1

--without optional determiner
makeDetBar2 :: InputData -> Int  -> RVar DetBar
makeDetBar2 idata n =
  DetBar (NoOpt) <$> (makeNounP idata i)
  where i = n-1

--CompBar
makeCompBar :: InputData -> Int  -> RVar CompBar
makeCompBar idata n =
  CompBar <$> (makeComp idata) <*> (makeTenseP idata i)
  where i = n-1

--TenseBar
makeTenseBar :: InputData -> Int  -> RVar TenseBar
makeTenseBar idata n =
  TenseBar <$> (makeTense idata) <*> (makeVerbP idata i)
  where i = n-1

--NounP
makeNounP :: InputData -> Int  -> RVar NounP
makeNounP idata n =
  NounP <$> (makeNounBar idata i)
  where i = n-1

--VerbP
makeVerbP :: InputData -> Int  -> RVar VerbP
makeVerbP idata n =
  VerbP <$> (makeVerbBar idata i)
  where i = n-1

--AdjP
makeAdjP :: InputData -> Int  -> RVar AdjP
makeAdjP idata n =
  AdjP <$> (makeAdjBar idata i)
  where i = n-1

--AdvP main
makeAdvP :: InputData -> Int  -> RVar AdvP
makeAdvP idata n
  | n <= 0    = join $ choice [makeAdvP2 idata 0]
  | otherwise = join $ choice [makeAdvP1 idata i, makeAdvP2 idata i]
  where i = n-1

--AdvBar with optional AdvP
makeAdvP1 :: InputData -> Int  -> RVar AdvP
makeAdvP1 idata n =
  AdvP <$> (YesOpt <$> (makeAdvP idata i)) <*> (makeAdvBar idata i)
  where i = n-1

--AdvBar without optional AdvP
makeAdvP2 :: InputData -> Int  -> RVar AdvP
makeAdvP2 idata n =
  (AdvP NoOpt) <$> (makeAdvBar idata i)
  where i = n-1

--PrepP main
makePrepP :: InputData -> Int  -> RVar PrepP
makePrepP idata n
  | n <= 0    = join $ choice [makePrepP2 idata 0]
  | otherwise = join $ choice [makePrepP1 idata i, makePrepP2 idata i]
  where i = n-1

--with optional AdjP
makePrepP1 :: InputData -> Int  -> RVar PrepP
makePrepP1 idata n =
  PrepP <$> (YesOpt <$> (makeAdjP idata i)) <*> (makePrepBar idata i)
  where i = n-1

--without optional AdjP
makePrepP2 :: InputData -> Int  -> RVar PrepP
makePrepP2 idata n =
  PrepP (NoOpt) <$> (makePrepBar idata i)
  where i = n-1

--DetP
makeDetP :: InputData -> Int  -> RVar DetP
makeDetP idata n =
  DetP <$> (makeDetBar idata i)
  where i = n-1

--CompP
makeCompP :: InputData -> Int  -> RVar CompP
makeCompP idata n =
  CompP <$> (makeCompBar idata i)
  where i = n-1

--TenseP
makeTenseP :: InputData -> Int  -> RVar TenseP
makeTenseP idata n =
  TenseP <$> (makeDetP idata i) <*> (makeTenseBar idata i)
  where i = n-1

--Parse the tree and output full string
parseTenseP :: TenseP -> String
parseTenseP tensep = str where
  TenseP detp tensebar = tensep
  str = parseDetP detp ++ " " ++ parseTenseBar tensebar

parseCompP :: CompP -> String
parseCompP compp = str where
  CompP compbar = compp
  str = parseCompBar compbar

parseDetP :: DetP -> String
parseDetP detp = str where
  DetP detbar = detp
  str = parseDetBar detbar

parsePrepP :: PrepP -> String
parsePrepP prepp = str where
  PrepP optadjp prepbar = prepp
  fooBar (YesOpt adjp) = parseAdjP adjp ++ " " ++ parsePrepBar prepbar
  fooBar NoOpt = parsePrepBar prepbar
  str = fooBar optadjp

parseAdvP :: AdvP -> String
parseAdvP advp = str where
  AdvP optadvp advbar = advp
  fooBar (YesOpt advp) = parseAdvP advp ++ " " ++ parseAdvBar advbar
  fooBar NoOpt = parseAdvBar advbar
  str = fooBar optadvp

parseAdjP :: AdjP -> String
parseAdjP adjp = str where
  AdjP adjbar = adjp
  str = parseAdjBar adjbar

parseVerbP :: VerbP -> String
parseVerbP verbp = str where
  VerbP verbbar = verbp
  str = parseVerbBar verbbar

parseNounP :: NounP -> String
parseNounP nounp = str where
  NounP nounbar = nounp
  str = parseNounBar nounbar

parseTenseBar :: TenseBar -> String
parseTenseBar tensebar = str where
  TenseBar tense verbp = tensebar
  str = parseTense tense ++ " " ++ parseVerbP verbp

parseCompBar :: CompBar -> String
parseCompBar compbar = str where
  CompBar comp tensep = compbar
  str = parseComp comp ++ " " ++ parseTenseP tensep

parseDetBar :: DetBar -> String
parseDetBar detbar = str where
  DetBar optdet nounp = detbar
  fooBar (YesOpt det) = parseDet det ++ " " ++ parseNounP nounp
  fooBar NoOpt = parseNounP nounp
  str = fooBar optdet

parsePrepBar :: PrepBar -> String
parsePrepBar (PrepBar1 prepbar optprepp) = str where
  fooBar (YesOpt prepp) = parsePrepBar prepbar ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parsePrepBar prepbar
  str = fooBar optprepp
parsePrepBar (PrepBar2 prep detp) = str where
  str = parsePrep prep ++ " " ++ parseDetP detp

parseAdvBar :: AdvBar -> String
parseAdvBar advbar = str where
  AdvBar adv = advbar
  str = parseAdv adv

parseAdjBar :: AdjBar -> String
parseAdjBar (AdjBar1 advp adjbar) = str where
  str = parseAdvP advp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar2 adjp adjbar) = str where
  str = parseAdjP adjp ++ " " ++ parseAdjBar adjbar
parseAdjBar (AdjBar3 adjbar optprepp) = str where
  fooBar (YesOpt prepp) = parseAdjBar adjbar ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseAdjBar adjbar
  str = fooBar optprepp
parseAdjBar (AdjBar4 adj optprepp) = str where
  fooBar (YesOpt prepp) = parseAdj adj ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseAdj adj
  str = fooBar optprepp

parseVerbBar :: VerbBar -> String
parseVerbBar (VerbBar1 advp verbbar) = str where
  str = parseAdvP advp ++ " " ++ parseVerbBar verbbar
parseVerbBar (VerbBar2 verbbar prepp) = str where
  str = parseVerbBar verbbar ++ " " ++ parsePrepP prepp
parseVerbBar (VerbBar3 verbbar advp) = str where
  str = parseVerbBar verbbar ++ " " ++ parseAdvP advp
parseVerbBar (VerbBar4 verb compp) = str where
  str = parseVerb verb ++ " " ++ parseCompP compp
parseVerbBar (VerbBar5 verb nounp) = str where
  str = parseVerb verb ++ " " ++ parseNounP nounp

parseNounBar :: NounBar -> String
parseNounBar (NounBar1 adjp nounbar) = str where
  str = parseAdjP adjp ++ " " ++ parseNounBar nounbar
parseNounBar (NounBar2 nounbar prepp) = str where
  str = parseNounBar nounbar ++ " " ++ parsePrepP prepp
parseNounBar (NounBar3 noun optprepp) = str where
  fooBar (YesOpt prepp) = parseNoun noun ++ " " ++ parsePrepP prepp
  fooBar NoOpt = parseNoun noun
  str = fooBar optprepp

parseTense :: Tense -> String
parseTense (Tense str) = str

parseComp :: Comp -> String
parseComp (Comp str) = str

parseDet :: Det -> String
parseDet (Det str) = str

parsePrep :: Prep -> String
parsePrep (Prep str) = str

parseAdv :: Adv -> String
parseAdv (Adv str) = str

parseAdj :: Adj -> String
parseAdj (Adj str) = str

parseVerb :: Verb -> String
parseVerb (Verb str) = str

parseNoun :: Noun -> String
parseNoun (Noun str) = str
