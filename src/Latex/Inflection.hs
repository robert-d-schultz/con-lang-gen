{-# LANGUAGE Rank2Types #-}
module Latex.Inflection
( writeLatexInflection
) where

import ClassyPrelude hiding ((<>))
import Text.LaTeX
import GHC.Exts

import Data.List as R (replicate)

import Data.Language
import Data.Word
import Data.Inflection

import Gen.Morpheme

import Out.Lexicon


writeLatexInflection :: Language -> LaTeX
writeLatexInflection lang = subsection (raw "Inflection")
                            <> raw "Inflection description here"
                            <> mconcat (writeInflectionTable lang <$> lcs <*> mts <*> os) where

  mts = [Particle .. CTransfix]
  lcs = [Verb .. Pron]
  os  = ordNub $ map (getOrder.getMeaning) (getInflMorphemes lang)

writeInflectionTable :: Language -> LexCat -> MorphType -> Int -> LaTeX
writeInflectionTable lang lc morphType i = out where
  gce@(GramCatExpresses gens anis cass nums defs spes tops pers hons pols tens asps moos vois evis tras vols) = cleanInflectionSys (getInflMap lang) lc morphType i
  relMorphs = filter (\m -> getMorphType m == morphType && getLC (getMeaning m) == lc && getOrder (getMeaning m) == i) (getInflMorphemes lang)
  (verListGen, horListGen, morphArrayGen, horSortingGen) = someFunction gens getGen ([], [], [relMorphs], id)
  (verListAni, horListAni, morphArrayAni, horSortingAni) = someFunction anis getAni (verListGen, horListGen, morphArrayGen, horSortingGen)
  (verListCas, horListCas, morphArrayCas, horSortingCas) = someFunction cass getCas (verListAni, horListAni, morphArrayAni, horSortingAni)
  (verListNum, horListNum, morphArrayNum, horSortingNum) = someFunction nums getNum (verListCas, horListCas, morphArrayCas, horSortingCas)
  (verListDef, horListDef, morphArrayDef, horSortingDef) = someFunction defs getDef (verListNum, horListNum, morphArrayNum, horSortingNum)
  (verListSpe, horListSpe, morphArraySpe, horSortingSpe) = someFunction spes getSpe (verListDef, horListDef, morphArrayDef, horSortingDef)
  (verListTop, horListTop, morphArrayTop, horSortingTop) = someFunction tops getTop (verListSpe, horListSpe, morphArraySpe, horSortingSpe)
  (verListPer, horListPer, morphArrayPer, horSortingPer) = someFunction pers getPer (verListTop, horListTop, morphArrayTop, horSortingTop)
  (verListHon, horListHon, morphArrayHon, horSortingHon) = someFunction hons getHon (verListPer, horListPer, morphArrayPer, horSortingPer)
  (verListPol, horListPol, morphArrayPol, horSortingPol) = someFunction pols getPol (verListHon, horListHon, morphArrayHon, horSortingHon)
  (verListTen, horListTen, morphArrayTen, horSortingTen) = someFunction tens getTen (verListPol, horListPol, morphArrayPol, horSortingPol)
  (verListAsp, horListAsp, morphArrayAsp, horSortingAsp) = someFunction asps getAsp (verListTen, horListTen, morphArrayTen, horSortingTen)
  (verListMoo, horListMoo, morphArrayMoo, horSortingMoo) = someFunction moos getMoo (verListAsp, horListAsp, morphArrayAsp, horSortingAsp)
  (verListVoi, horListVoi, morphArrayVoi, horSortingVoi) = someFunction vois getVoi (verListMoo, horListMoo, morphArrayMoo, horSortingMoo)
  (verListEvi, horListEvi, morphArrayEvi, horSortingEvi) = someFunction evis getEvi (verListVoi, horListVoi, morphArrayVoi, horSortingVoi)
  (verListTra, horListTra, morphArrayTra, horSortingTra) = someFunction tras getTra (verListEvi, horListEvi, morphArrayEvi, horSortingEvi)
  (verListFinal, horListFinal, morphArrayVol, horSortingFinal) = someFunction vols getVol (verListTra, horListTra, morphArrayTra, horSortingTra)
  morphArrayFinal = map horSortingFinal morphArrayVol
  body
    | morphType == Suffix = map (map (\m -> "–" ++ writeMorphemeIPA lang m)) morphArrayFinal
    | morphType == Prefix = map (map (\m -> writeMorphemeIPA lang m ++ "–")) morphArrayFinal
    | otherwise =  map (map (writeMorphemeIPA lang)) morphArrayFinal

  verH = R.replicate (length horListFinal) " & " ++ verHeader horListFinal verListFinal
  horH = horHeader horListFinal
  foo =  horH ++ map (intercalate  " & ") body
  bar | null verListFinal = foo
      | otherwise = zipWith (++) verH foo

  caption = "caption = " ++ tshow lc ++ " " ++ tshow morphType ++ " " ++ tshow i ++ " Inflection Table"
  options = caption ++ ", center" ++ ", mincapwidth = 40mm"
  colStuff = "|c|*{" ++ tshow (length verListFinal + product (map length horListFinal)-1) ++ "}{c|}"
  notes = ""
  table = "\\hline" ++ intercalate " \\\\ \\hline " bar ++  "\\\\ \\hline"
  out | gce == GramCatExpresses [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress] [NoExpress]
          = mempty
      | otherwise = raw ("\\ctable[" ++ options ++ "]" ++ "{" ++ colStuff ++ "}" ++ "{" ++ notes ++ "}" ++ "{" ++ table ++ "}")


horHeader :: [[Text]] -> [Text]
horHeader [] = []
horHeader [ts] = ["\\rot{" ++ intercalate "} & \\rot{" ts ++ "}"]
horHeader (ts:tss) = concatMap (\t -> "\\multicolumn{" ++ tshow (product $ map length tss) ++ "}{c|}{\\rot{" ++ tshow t ++ "}}") ts
                   : map (intercalate " & " . R.replicate (length ts)) (horHeader tss)

verHeader :: [[Text]] -> [[Text]] -> [Text]
verHeader _ [] = []
verHeader hs [ts] = map (++ " & ") ts
verHeader hs (ts:tss) = zipWith (++) col (R.replicate (length hs) " & " ++ verHeader hs tss) where
  col = map (\t -> "\\multirow{" ++ tshow (product $ map length tss) ++ "}{*}{" ++ tshow t ++ "} & " ) ts
     ++ R.replicate (product (map length tss) - 1) " & "

someFunction :: (Ord a, Eq a, GramCat a, Show a) => [Express a] -> (GramCatExpress -> Express a) -> ([[Text]],[[Text]],[[Morpheme]],[Morpheme] -> [Morpheme]) -> ([[Text]],[[Text]],[[Morpheme]],[Morpheme] -> [Morpheme])
someFunction [] _ old = old
someFunction [NoExpress] _ old = old
someFunction gs gf (oldVerList, oldHorList, oldMorphArray, oldHorSortingF) = out where
-- decide vertical or horizontal
  out
    -- vertical
    | product (map length oldVerList) < product (map length oldHorList) = (oldVerList ++ [map tshow gs], oldHorList, verFoobar gf oldMorphArray, oldHorSortingF)
    -- horizontal
    | otherwise = (oldVerList, oldHorList ++ [map tshow gs], oldMorphArray, newHorSortingF)

  newHorSortingF = oldHorSortingF . sortBy (\a b -> compare (getExp $ gf $ getAllExpress $ getMeaning a) (getExp $ gf $ getAllExpress $ getMeaning b))

verFoobar :: (Ord a, GramCat a) => (GramCatExpress -> Express a) -> [[Morpheme]] -> [[Morpheme]]
verFoobar gf = concatMap (groupWith f) where
  f a = gf (getAllExpress $ getMeaning a)
