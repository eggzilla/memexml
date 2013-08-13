-- This module contains a hierarchical data structure for MEME results

module Bio.MemeData where
--import Bio.Core
--import Data.ByteString.Lazy.Char8 (ByteString)


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
  { alphabet :: String
  , ambigs :: String
  , sequence :: [Sequence]
  , letter_frequencies :: String}
  deriving (Show, Eq)

data Sequence = Sequence
  { sequenceId :: String
  , sequenceName :: String
  , sequenceLength :: String}
  deriving (Show, Eq)
  
-- Description of Processing
data Model = Model
  { command_line :: String
  , host :: String
  , model_type :: String
  , nmotifs :: String
  , evalue_threshold :: String
  , object_function :: String
  , min_width :: String
  , max_width :: String
  , minic :: String
  , wg :: String
  , ws :: String
  , endgaps :: String
  , minsites :: String
  , maxsites :: String
  , wnsites :: String
  , prob :: String
  , spmap :: String
  , spfuzz :: String
  , prior :: String
  , beta :: String
  , maxiter :: String
  , distance :: String
  , num_sequences :: String
  , num_positions :: String
  , seed :: String
  , seqfrac :: String
  , strands :: String
  , priors_file :: String
  , reason_for_stopping :: String
  , background_frequencies :: String}
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
