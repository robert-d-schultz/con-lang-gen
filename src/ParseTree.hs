module ParseTree(


) where

data Optional a = Present a | Adbsent

data Sentence = Sentence NounPhrase VerbPhrase

data VerbPhrase = VerbPhrase Verb (Optional NounPhrase) (Optional Adj)

data NounPhrase = NounPhrase Noun (Optional Adj) (Optional PrepPhrase)

data

data PrepPhrase = PrepositionPhrase Prep (Either NounPhrase PrepPhrase)

data AdjPhrase = AdjPhrase Adj Adv

data AdvPhrase = AdvPhrase Adv VerbPhrase

data DeterminerPhrase = DeterminerPhrase Derminer NounPhrase

data Adv = Adv String
data Adj = Adj String
data Prep = Prep String
data Verb = Verb String
data Noun = Noun String
data Coordinate = Coordinate String
data Determiner = Determiner String
data Negation = Negation String
data Particle = Particle String
data Subordinate = Subordinate String
