module Out.Phonology
( writeConPhonemeInventory
, writeVowPhonemeInventory
, writeDiphPhonemeInventory
, writeToneInventory
, writePhonationInventory
, writeRoundednessInventory
) where

import ClassyPrelude

import Data.Phoneme
import Out.Lexicon

-- Write the consonant inventory into html tables (one for each airstream mechanism)
writeConPhonemeInventory :: [Phoneme] -> Text
writeConPhonemeInventory cns = "\n<br>\n" ++ intercalate "\n<br>\n" out where
  airstreams = sort $ ordNub $ map getAirstream cns
  out = map (writeConPhonemeInventory_ cns) airstreams


writeConPhonemeInventory_ :: [Phoneme] -> Airstream -> Text
writeConPhonemeInventory_ cns airstream = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
  cns_ = filter (\x -> airstream == getAirstream x) cns
  places = sort $ ordNub $ map getPlace cns_
  manners = sort $ ordNub $ map getManner cns_
  phonations = sort $ ordNub $ map getVoice cns_
  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (length places * length phonations + 1) ++ "\">" ++ tshow airstream ++ " Consonants</th>\n\t</tr>"
  pLabels = "\n\t<tr>\n\t\t<th></th>\n\t\t<th colspan=\"" ++ tshow (length phonations) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ tshow (length phonations) ++ "\">") (map tshow places) ++ "</th>\n\t</tr>"
  clusters = concatMap (makeRow cns airstream places phonations) manners

makeRow :: [Phoneme] -> Airstream -> [Place] -> [Phonation] -> Manner -> Text
makeRow cns airstream places phonations manner = row where
  row = "\n\t<tr>\n\t\t<th>" ++ tshow manner ++ "</th>" ++ cluster ++ "\n\t</tr>"
  cluster = concatMap (makeCluster cns airstream manner phonations) places

makeCluster :: [Phoneme] -> Airstream -> Manner -> [Phonation] -> Place -> Text
makeCluster cns airstream manner phonations place = cluster where
  cluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" (map (getIPASymbol cns airstream manner place) phonations) ++ "</td>"

getIPASymbol :: [Phoneme] -> Airstream -> Manner -> Place -> Phonation -> Text
getIPASymbol cns airstream manner place phonation = fromMaybe " " (writePhonemeIPA <$> filt) where
  filt = find (\(Consonant p m h a) -> p == place && m == manner && h == phonation && a == airstream) cns


-- write the vowel inventory into html table
writeVowPhonemeInventory :: [Phoneme] -> Text
writeVowPhonemeInventory vows = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
  heights = sort $ ordNub $ map getHeight vows
  backs = sort $ ordNub $ map getBackness vows
  rounds = sort $ ordNub $ map getRoundedness vows
  lengths = sort $ ordNub $ map getLength vows

  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (length backs * length rounds + 1) ++ "\">Vowel Inventory</th>\n\t</tr>"
  pLabels = "\n\t<tr>\n\t\t<th></th>\n\t\t<th colspan=\"" ++ tshow (length rounds) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ tshow (length rounds) ++ "\">") (map tshow backs) ++ "</th>\n\t</tr>"
  clusters = concatMap (makeSuperRow vows backs rounds lengths) heights

makeSuperRow :: [Phoneme] -> [Backness] -> [Roundedness] -> [Length] -> Height -> Text
makeSuperRow vows backs rounds lengths height = superrow where
  superrow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ tshow (length lengths + 1) ++ "\">" ++ tshow height ++ "</th>" ++ rows ++ "\n\t</tr>"
  rows = concatMap (makeRow2 vows rounds backs height) lengths

makeRow2 :: [Phoneme] -> [Roundedness] -> [Backness] -> Height -> Length -> Text
makeRow2 vows rounds backs height len = cluster where
  cluster = "\n\t\t<tr>" ++ concatMap (makeCluster2 vows len height rounds) backs ++ "\n\t\t</tr>"

makeCluster2 :: [Phoneme] ->  Length -> Height -> [Roundedness] -> Backness -> Text
makeCluster2 vows len height rounds back  = cluster where
  cluster = "\n\t\t\t<td>" ++ intercalate "</td>\n\t\t\t<td>" (map (getIPASymbol2 vows len height back) rounds) ++ "</td>"

getIPASymbol2 :: [Phoneme] -> Length -> Height -> Backness -> Roundedness -> Text
getIPASymbol2 vows len height back roundness = fromMaybe " " (writePhonemeIPA <$> f) where
  f = find (\(Vowel h b r l) -> h == height && b == back && r == roundness && l == len) vows

-- write the diphthong inventory, should be a table in the final
writeDiphPhonemeInventory :: [Phoneme] -> Text
writeDiphPhonemeInventory [] = "<br>\nDiphthongs: None"
writeDiphPhonemeInventory [d] = "<br>\nDiphthongs: /" ++ writePhonemeIPA d ++ "/\n"
writeDiphPhonemeInventory (d:ds) = "<br>\nDiphthongs: /" ++ intercalate "/, /" (map writePhonemeIPA ds) ++ "/, and /" ++ writePhonemeIPA d ++ "/\n"

-- write out what the tones are
writeToneInventory :: [Tone] -> Text
writeToneInventory [] = "<br>\nTones: None"
writeToneInventory [NONET] = "<br>\nTones: none"
writeToneInventory [t] = "<br>\nTones: " ++ tshow t ++ "\n"
writeToneInventory ts = fromMaybe "" foobar where
  foobar = do
            foo <- initMay ts
            bar <- lastMay ts
            return $ "<br>\nTones: " ++ intercalate ", " (map tshow foo) ++ ", and " ++ tshow bar ++ "\n"

-- write out what the phonations are
writePhonationInventory :: [Phonation] -> Text
writePhonationInventory [] = "<br>\n"
writePhonationInventory [_] = "<br>\n"
writePhonationInventory [h1, h2] = "<br>\nSymbols on the left in a cell are " ++ tshow h1 ++ ", to the right are " ++ tshow h2 ++ ".\n<br>\n"
writePhonationInventory hs = fromMaybe "" foobar where
  foobar = do
              foo <- initMay hs
              bar <- lastMay hs
              return $ "<br>\nPhonations from left to right: " ++ intercalate ", " (map tshow foo) ++ ", and " ++ tshow bar ++ "\n<br>\n"

writeRoundednessInventory :: [Roundedness] -> Text
writeRoundednessInventory [] = "<br>\n"
writeRoundednessInventory [_] = "<br>\n"
writeRoundednessInventory [r1, r2] = "<br>\nPaired vowels are: " ++ tshow r1 ++ ", " ++ tshow r2 ++ "\n"
writeRoundednessInventory rs = fromMaybe "" foobar where
  foobar = do
              foo <- initMay rs
              bar <- lastMay rs
              return $ "<br>\nRoundedness from left to right: " ++ intercalate ", " (map tshow foo) ++ ", and " ++ tshow bar ++ "\n" --unlikely to use
