module Latex.Phonology
( writeLatexPhonology
) where

import ClassyPrelude hiding ((<>))
import Text.LaTeX
import GHC.Exts

import Data.Language
import Data.Phoneme

import Out.Lexicon
import Out.IPA

writeLatexPhonology :: Language -> LaTeX
writeLatexPhonology lang = section (raw "Phonology")
                           <> raw "Phonology description here"
                           <> writeConsonantTables lang
                           <> writeVowelTable lang
                           <> writeToneTable lang

writeConsonantTables :: Language -> LaTeX
writeConsonantTables lang = out where
  cns = getCInv lang
  airstreams = sort $ ordNub $ map getAirstream cns
  out = mconcat (map (writeConsonantTables_ lang) airstreams)

writeConsonantTables_ :: Language -> Airstream -> LaTeX
writeConsonantTables_ lang airstream = out where
  cns = getCInv lang
  cns_ = filter (\x -> airstream == getAirstream x) cns
  places = sort $ ordNub $ map getPlace cns_
  manners = sort $ ordNub $ map getManner cns_
  phonations = sort $ ordNub $ map getVoice cns_

  hor = " & " ++ "\\multicolumn{" ++ tshow (length phonations) ++ "}{c|}{\\rot{" ++ intercalate ("}} & \\multicolumn{" ++ tshow (length phonations) ++ "}{c|}{\\rot{") (map tshow places) ++ "}}" ++ " \\\\ \\hline "
  clusters = concatMap (makeRowC cns_ airstream places phonations) manners


  caption | null $ writePhonationInventory phonations = "caption = " ++ tshow airstream ++ " Consonant Table"
          | otherwise = "caption = " ++ tshow airstream ++ " Consonant Table" ++ "\\tmark"
  options = caption ++ ", center" ++ ", mincapwidth = 40mm"
  colStuff = "|c|*{" ++ tshow (length phonations * length places) ++ "}{c|}"
  notes | null $ writePhonationInventory phonations = ""
        | otherwise = "\\tnote{" ++ writePhonationInventory phonations ++ "}"
  table = "\\hline" ++ hor ++ clusters
  out = raw ("\\ctable[" ++ options ++ "]" ++ "{" ++ colStuff ++ "}" ++ "{" ++ notes ++ "}" ++ "{" ++ table ++ "}")



makeRowC :: [Phoneme] -> Airstream -> [Place] -> [Phonation] -> Manner -> Text
makeRowC cns airstream places phonations manner = row where
  row = tshow manner ++ cluster ++ " \\\\ \\hline "
  cluster = concatMap (makeClusterC cns airstream manner phonations) places

  makeClusterC :: [Phoneme] -> Airstream -> Manner -> [Phonation] -> Place -> Text
  makeClusterC cns airstream manner phonations place = cluster where
    cluster = " & " ++ intercalate " & " (map (getIPASymbolC cns airstream manner place) phonations)

    getIPASymbolC :: [Phoneme] -> Airstream -> Manner -> Place -> Phonation -> Text
    getIPASymbolC cns airstream manner place phonation = fromMaybe " " (writePhonemeIPA <$> filt) where
      filt = find (\(Consonant p m h a) -> p == place && m == manner && h == phonation && a == airstream) cns


writeVowelTable :: Language -> LaTeX
writeVowelTable lang = out where
  vows = getVInv lang
  heights = sort $ ordNub $ map getHeight vows
  backs = sort $ ordNub $ map getBackness vows
  rounds = sort $ ordNub $ map getRoundedness vows
  lengths = sort $ ordNub $ map getLength vows

  hor = " & " ++ "\\multicolumn{" ++ tshow (length rounds) ++ "}{c|}{\\rot{" ++ intercalate ("}} & \\multicolumn{" ++ tshow (length rounds) ++ "}{c|}{\\rot{") (map tshow backs) ++ "}}" ++ " \\\\ \\hline "
  clusters = concatMap (makeSuperRowV vows backs rounds lengths) heights


  caption | null (writeRoundednessInventory rounds ++ writeLengthsInventory lengths) = "caption = Vowel Table"
          | otherwise = "caption = Vowel Table" ++ "\\tmark"
  options = caption ++ ", center" ++ ", mincapwidth = 40mm"
  colStuff = "|c|*{" ++ tshow (length rounds * length backs) ++ "}{c|}"
  notes | null $ writeRoundednessInventory rounds = ""
        | otherwise = "\\tnote{" ++ writeRoundednessInventory rounds ++ " " ++ writeLengthsInventory lengths ++ "}"
  table = "\\hline" ++ hor ++ clusters
  out = raw ("\\ctable[" ++ options ++ "]" ++ "{" ++ colStuff ++ "}" ++ "{" ++ notes ++ "}" ++ "{" ++ table ++ "}")



makeSuperRowV :: [Phoneme] -> [Backness] -> [Roundedness] -> [Length] -> Height -> Text
makeSuperRowV vows backs rounds lengths height = superrow where
  superrow = "\\multirow{" ++ tshow (length lengths) ++ "}{*}{" ++ tshow height ++ "}" ++ rows_ ++ " \\\\ \\hline "
  rows = map (makeRowV vows rounds backs height) lengths
  rows_ = intercalate (" \\\\ \\cline{2-" ++ tshow (length rounds * length backs + 1) ++ "}") rows

  makeRowV :: [Phoneme] -> [Roundedness] -> [Backness] -> Height -> Length -> Text
  makeRowV vows rounds backs height len = cluster where
    cluster = concatMap (makeClusterV vows len height rounds) backs

    makeClusterV :: [Phoneme] ->  Length -> Height -> [Roundedness] -> Backness -> Text
    makeClusterV vows len height rounds back  = cluster where
      cluster = " & " ++ intercalate " & " (map (getIPASymbolV vows len height back) rounds)

      getIPASymbolV :: [Phoneme] -> Length -> Height -> Backness -> Roundedness -> Text
      getIPASymbolV vows len height back roundness = fromMaybe " " (writePhonemeIPA <$> f) where
        f = find (\(Vowel h b r l) -> h == height && b == back && r == roundness && l == len) vows


-- This should create a table, tone symbol applied to placeholder
writeToneTable :: Language -> LaTeX
writeToneTable lang = out where
  tons = getTones lang
  caption = "caption = Tone Table"
  options = caption ++ ", center" ++ ", mincapwidth = 40mm"
  colStuff = "|c|*{" ++ tshow (length tons - 1) ++ "}{c|}"
  notes  = ""
  table = "\\hline" ++ intercalate " & " (map writeToneLetterIPA tons) ++ "\\\\ \\hline" ++ intercalate " & " (map tshow tons) ++ "\\hline"
  out | null tons || tons == [NONET] = mempty
      | otherwise = raw ("\\ctable[" ++ options ++ "]" ++ "{" ++ colStuff ++ "}" ++ "{" ++ notes ++ "}" ++ "{" ++ table ++ "}")


-- In table footnotes
writePhonationInventory :: [Phonation] -> Text
writePhonationInventory [] = ""
writePhonationInventory [_] = ""
writePhonationInventory [h1, h2] = "Consonants on the left are " ++ tshow h1 ++ ", to the right are " ++ tshow h2 ++ "."
writePhonationInventory hs = fromMaybe "" foobar where
  foobar = do
              foo <- initMay hs
              bar <- lastMay hs
              return $ "Phonations from left to right: " ++ intercalate ", " (map tshow foo) ++ ", and " ++ tshow bar ++ "."

-- In table footnotes
writeRoundednessInventory :: [Roundedness] -> Text
writeRoundednessInventory [] = ""
writeRoundednessInventory [_] = ""
writeRoundednessInventory [r1, r2] = "Vowels on the left are " ++ tshow r1 ++ ", to the right are " ++ tshow r2 ++ "."
writeRoundednessInventory rs = fromMaybe "" foobar where
  foobar = do
              foo <- initMay rs
              bar <- lastMay rs
              return $ "Roundedness from left to right: " ++ intercalate ", " (map tshow foo) ++ ", and " ++ tshow bar ++ "." --unlikely to use

-- In table footnotes
writeLengthsInventory :: [Length] -> Text
writeLengthsInventory [] = ""
writeLengthsInventory [_] = ""
writeLengthsInventory [l1, l2] = "Vowels on the top are " ++ tshow l1 ++ ", on the bottom are " ++ tshow l2 ++ "."
writeLengthsInventory ls = fromMaybe "" foobar where
  foobar = do
              foo <- initMay ls
              bar <- lastMay ls
              return $ "Vowel length from top to bottom: " ++ intercalate ", " (map tshow foo) ++ ", and " ++ tshow bar ++ "."
