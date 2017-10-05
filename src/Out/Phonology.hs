{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Out.Phonology
( writeConPhonemeInventory
, writeVowPhonemeInventory
, writeDiphPhonemeInventory
) where

import ClassyPrelude hiding (Word)

import Data.Phoneme
import Out.Lexicon
import Out.IPA

-- write the consonant inventory into html table
writeConPhonemeInventory :: [Phoneme] -> Text
writeConPhonemeInventory cons = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
  places = sort $ ordNub $ map cplace cons
  manners = sort $ ordNub $ map cmanner cons
  phonations = sort $ ordNub $ map cvoice cons
  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (length places * length phonations + 1) ++ "\">Consonant Inventory</th>\n\t</tr>"
  pLabels = "\n\t<tr>\n\t\t<th></th>\n\t\t<th colspan=\"" ++ tshow (length phonations) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ tshow (length phonations) ++ "\">") (map tshow places) ++ "</th>\n\t</tr>"
  clusters = concatMap (makeRow cons places phonations) manners

  makeRow :: [Phoneme] -> [Place] -> [Phonation] -> Manner -> Text
  makeRow cons places phonations manner = row where
    row = "\n\t<tr>\n\t\t<th>" ++ tshow manner ++ "</th>" ++ cluster ++ "\n\t</tr>"
    cluster = concatMap (makeCluster cons manner phonations) places

  makeCluster :: [Phoneme] -> Manner -> [Phonation] -> Place -> Text
  makeCluster cons manner phonations place = cluster where
    cluster = "\n\t\t<td>" ++ intercalate "</td>\n\t\t<td>" (map (getIPASymbol cons manner place) phonations) ++ "</td>"

  getIPASymbol :: [Phoneme] -> Manner -> Place -> Phonation -> Text
  getIPASymbol cons manner place phonation = fromMaybe "" (writePhonemeIPA <$> filt) where
    filt = find (\(Consonant p m h) -> p == place && m == manner && h == phonation) cons


-- write the vowel inventory into html table
writeVowPhonemeInventory :: [Phoneme] -> Text
writeVowPhonemeInventory vows = "<br>\n<table border=1>" ++ tHeader ++ pLabels ++ clusters ++ "\n</table>\n" where
  heights = sort $ ordNub $ map vheight vows
  backs = sort $ ordNub $ map vbackness vows
  rounds = sort $ ordNub $ map vroundedness vows
  lengths = sort $ ordNub $ map vlength vows

  tHeader = "\n\t<tr>\n\t\t<th colspan=\"" ++ tshow (length backs * length rounds + 1) ++ "\">Vowel Inventory</th>\n\t</tr>"
  pLabels = "\n\t<tr>\n\t\t<th></th>\n\t\t<th colspan=\"" ++ tshow (length rounds) ++ "\">" ++ intercalate ("</th>\n\t\t<th colspan=\"" ++ tshow (length rounds) ++ "\">") (map tshow backs) ++ "</th>\n\t</tr>"
  clusters = concatMap (makeSuperRow vows backs rounds lengths) heights

  makeSuperRow :: [Phoneme] -> [Backness] -> [Roundedness] -> [Length] -> Height -> Text
  makeSuperRow vows backs rounds lengths height = superrow where
    superrow = "\n\t<tr>\n\t\t<th rowspan=\"" ++ tshow (length lengths + 1) ++ "\">" ++ tshow height ++ "</th>" ++ rows ++ "\n\t</tr>"
    rows = concatMap (makeRow vows rounds backs height) lengths

  makeRow :: [Phoneme] -> [Roundedness] -> [Backness] -> Height -> Length -> Text
  makeRow vows rounds backs height len = cluster where
    cluster = "\n\t\t<tr>" ++ concatMap (makeCluster vows len height rounds) backs ++ "\n\t\t</tr>"

  makeCluster :: [Phoneme] ->  Length -> Height -> [Roundedness] -> Backness -> Text
  makeCluster vows len height rounds back  = cluster where
    cluster = "\n\t\t\t<td>" ++ intercalate "</td>\n\t\t\t<td>" (map (getIPASymbol vows len height back) rounds) ++ "</td>"

  getIPASymbol :: [Phoneme] -> Length -> Height -> Backness -> Roundedness -> Text
  getIPASymbol vows len height back roundness = fromMaybe "" (writePhonemeIPA <$> ((\x -> x{vtone=NONET}) <$> f)) where
    f = find (\(Vowel h b r l _) -> h == height && b == back && r == roundness && l == len) vows

-- write the diphthong inventory, should be a table in the final
writeDiphPhonemeInventory :: [Phoneme] -> Text
writeDiphPhonemeInventory [] = "<br>\nDiphthongs: none"
writeDiphPhonemeInventory [d] = "<br>\nDiphthongs: /" ++ writePhonemeIPA d ++ "/\n"
writeDiphPhonemeInventory (d:ds) = "<br>\nDiphthongs: /" ++ intercalate "/, /" (map writePhonemeIPA ds) ++ "/, and /" ++ writePhonemeIPA d ++ "/\n"
