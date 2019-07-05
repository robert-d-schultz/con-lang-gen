module Gen.Meaning
(
) where

import ClassyPrelude hiding (Word)
import Data.RVar
import Data.Random.Extras
import Data.Random hiding (sample)

-- WordNet plan
-- Need to use python to build a special database from WordNet
-- Readable by Haskell, binary format maybe
-- Hyponyms and hypernyms are the most important
-- Use some subset, remove all "instances", remove recent techonology, only the most common 10,000 senses, etc.

-- data will look like this:
-- data SynSetNoun = { id :: Text, word :: Text, definition :: Text, hypernyms :: [Text], hyponyms :: [Text] }
-- data SynSetVerb = { id :: Text, word :: Text, definition :: Text, hypernyms :: [Text], troponyms :: [Text] }
-- data SynSetAdj  = { id :: Text, word :: Text, definition :: Text, similar :: [Text], antonym :: Maybe Text }

-- when filling out the lexicon,
-- need to allow some senses and their hyponyms to be the same word
-- like imagine if "seat" could refer to seats in general as well as a specific type of seat like a chair

-- I'd probably start at the SynSets with no hyponyms, generate those
-- Then generate their direct hypernyms, allowing a small chance to "inherit"
-- That should work out

-- There's a problem with how to handle derivational morphology
-- There are pairs of senses that are derivationally related to each other
-- For instance, "seat" like a chair, and the verb "seat" to show someone to a seat.
-- A generated language could have many morphemes for derivation
-- But there's no connection between the output of Synset+DerMorpheme and a Synset in our dictionary
-- This would result in duplicate entries in the final lexicon, having an entry for the noun "investment" and another entry for "invest (verb) + some morpheme that means "act of"", when really it's the same sense
-- I'm not quite sure how to fix this
-- Two derivationally linked senses are connected, but the type of connection is nebulous
-- It's just "derivationally related form"
-- So there's no way of knowing if the connection is like "act of" (verb -> noun) or something else
-- If a generated language has no derivational morphology, then we don't want to delete any of the senses
-- If a generated language has many derivational morphemes, we could delete all senses with "derivationally related forms", or at least half of them
--  This is a slight problem because many of the sense derivations are irregular, and not going to be "re"captured by the generated dervMorphs
--  It might be acceptable though if most of them will
-- But if a generated language only have 1 or two derivational morphemes, like only the "act of" (verb -> noun) one, it's tricky
--  Deleted all the "derivationally related form" senses would be a giant waste
--  But there's that duplication problem
--  So, have to live with one of them
--  Or else find a WordNet-like thing that tells you what the derivation connection is specifically

-- Hm there's a morphosemantic database for Wordnet, 14 semantic relations
-- Only noun-verb though
-- I could use that to delete some of the redundant senses...
-- Although maybe they shouldn't be deleted, just the words generated for those senses need to have the relevent morpheme
-- revised data
-- data SynSetNoun = { id :: SynSetId, word :: Text, definition :: Text, hypernyms :: [SynSetId], hyponyms :: [SynSetId], derivations :: [(SynSetId, Relation)] }
-- data SynSetVerb = { id :: SynSetId, word :: Text, definition :: Text, hypernyms :: [SynSetId], troponyms :: [Text], derivations :: [(SynSetId, Relation)] }
-- data SynSetAdj  = { id :: SynSetId, word :: Text, definition :: Text, similar :: [SynSetId], antonym :: Maybe SynSetId }
-- type SynSetId = Text
-- data Relation = Agent | BodyPart | ByMeansOf | Destination | Event | Instrument | Location | Material | Property | Result | State | Undergoer | Uses | Vehicle

-- Another interesting possiblity is "irregular" derivational morphology
-- The morphemes generated are good for regular dervMorph, but it's hard to capture the relations between words as regular
-- For instance we have the noun "seat" related to 4 "seat" different verbs, all relating to seats.
-- The relationships between the noun and its verbs are hard to describe, and are probably only applicable to seats
-- So it might be interesting to take the generated word for noun "seat" and have some of the generated words for those verbs be similar in someway to the noun

-- Gen.Meaning?
-- special generators
makeColorSystem :: RVar [Text]
makeColorSystem = do
  ncolors <- uniform 0 11 :: RVar Int
  case ncolors of
    0 -> return []
    1 -> sample 1 ["white", "black"]
    2 -> return ["white", "black"]
    3 -> return ["white", "black", "red"]
    4 -> (++) ["white", "black", "red"] <$> sample 1 ["yellow", "green"]
    5 -> return ["white", "black", "red", "yellow", "green"]
    6 -> return ["white", "black", "red", "yellow", "green", "blue"]
    7 -> return ["white", "black", "red", "yellow", "green", "blue", "brown"]
    8 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 1 ["purple", "pink", "orange", "gray"]
    9 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 2 ["purple", "pink", "orange", "gray"]
    10 -> (++) ["white", "black", "red", "yellow", "green", "blue", "brown"] <$> sample 3 ["purple", "pink", "orange", "gray"]
    11 -> return ["white", "black", "red", "yellow", "green", "blue", "brown", "pueple", "pink", "orange", "gray"]
    _ -> return []

makeNumberSystem :: RVar [Text]
makeNumberSystem = do
  base <- choice [2, 4, 5, 6, 8, 10, 12, 15, 16, 20, 40, 60]
  return $ map tshow ([0..base] ++ ((^) <$> [base] <*> [2..6]))
