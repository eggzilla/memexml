
module Bio.MemeData where

--import Bio.Core

data Sequence = Sequence
  { sequenceId :: String, sequenceName :: String, sequenceLength :: String}
    deriving (Show, Eq)

data Motif = Motif
  { motifId :: String, motifWidth :: String, motifRegularexpression :: String, motifContributingsites :: [ContributingSite]}
    deriving (Show, Eq)

data ContributingSite = ContributingSite
  { siteId :: String, sitePosition :: String, siteStrand :: String, sitePvalue :: String, siteSequence :: [LetterReference] }
    deriving (Show, Eq)

data LetterReference = LetterReference
  { letterReference :: String }
    deriving (Show, Eq)
              
