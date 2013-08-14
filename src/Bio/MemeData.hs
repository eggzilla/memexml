-- This module contains a hierarchical data structure for
-- Multiple EM for Motif Elicitation (MEME) results

module Bio.MemeData where

-- Document Root of Meme xml output
data MemeResult = MemeResult 
  { memeversion, memerelease :: String
  , trainingset :: TrainingSet
  , model :: Model
  , motifs :: [Motif]
  , scanned_site_summary :: ScannedSiteSummary }
  deriving (Show, Eq)

-- Description of input data
data TrainingSet = TrainingSet
  { datafile :: String
  , length :: Int
  , alphabet :: MemeAlphabet
  , ambigs :: [MemeLetter]
  , sequence :: [Sequence]
  , letter_frequencies :: LetterFrequencies}
  deriving (Show, Eq)

data MemeAlphabet = MemeAlphabet
  { memeAlphabetId :: String
  , memeAlphabetLength :: Int
  , letters :: [MemeLetter]}
  deriving (Show, Eq)     
         
data MemeLetter = MemeLetter
  { memeLetterId :: String
  , memeLetterSymbol :: Char}
  deriving (Show, Eq)

data MemeAmbigs = MemeAmbigs
  { ambigletters :: [MemeLetter]}
  deriving (Show, Eq)
           
data Sequence = Sequence
  { sequenceId :: String
  , sequenceName :: String
  , sequenceLength :: Int
  , sequenceWeight :: Double}
  deriving (Show, Eq)

data LetterFrequencies = LetterFrequencies
  { letterFrequenciesAlphabetArray :: AlphabetArray}
  deriving (Show, Eq)
           
-- Description of Processing
data Model = Model
  { command_line :: String
  , host :: String
  , model_type :: String
  , nmotifs :: Int
  , evalue_threshold :: String
  , object_function :: String
  , min_width :: Int
  , max_width :: Int
  , minic :: Double
  , wg :: Int
  , ws :: Int
  , endgaps :: String
  , minsites :: Int
  , maxsites :: Int
  , wnsites :: Double
  , prob :: Int
  , spmap :: String
  , spfuzz :: Double
  , prior :: String
  , beta :: Double
  , maxiter :: Int
  , distance :: Double
  , num_sequences :: Int
  , num_positions :: Int
  , seed :: Int
  , seqfrac :: String
  , strands :: String
  , priors_file :: String
  , reason_for_stopping :: String
  , background_frequencies :: BackgroundFrequencies}
  deriving (Show, Eq)        

data BackgroundFrequencies = BackgroundFrequencies
  { source :: String
  , backgroundFrequenciesAlphabetArray :: AlphabetArray}
  deriving (Show, Eq)

data AlphabetArray = AlphabetArray
  { alphabetArrayValues :: [AlphabetArrayValue]}
  deriving (Show, Eq)

data AlphabetArrayValue = AlphabetArrayValue
  { letterId :: String,
    frequency :: Double}
  deriving (Show, Eq)

     
--Detected motifs
data Motif = Motif
  { motifId :: String
  , motifWidth :: String
  , motifRegularexpression :: String
  , motifContributingsites :: [ContributingSite]}
  deriving (Show, Eq)

data ContributingSite = ContributingSite
  { contributingSiteId :: String
  , contributingSitePosition :: String
  , contributingSiteStrand :: String
  , contributingSitePvalue :: String
  , contributingSiteSequence :: [LetterReference] }
    deriving (Show, Eq)

data LetterReference = LetterReference
  { letterReference :: String }
  deriving (Show, Eq)
              
data ScannedSiteSummary = ScannedSiteSummary
  { p_thresh :: String
  , scannedSites :: [ScannedSites]}
  deriving (Show, Eq)
           
data ScannedSites = ScannedSites
  { sequence_id :: String              
  , scannedSitesPvalue :: Double
  , num_sites :: Int
  , scannedsite :: [ScannedSite]}
  deriving (Show, Eq)
           
data ScannedSite = ScannedSite
  { motif_id :: String              
  , strand :: String
  , position :: Int
  , scannedSitePvalue :: Double}
  deriving (Show, Eq)
