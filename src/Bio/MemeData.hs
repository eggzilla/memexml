-- | This module contains a hierarchical data structure for
--   Multiple EM for Motif Elicitation (MEME) results

module Bio.MemeData where

-- | Document Root of Meme xml output
data MemeResult = MemeResult 
  { memeversion, memerelease :: String
  , trainingset :: TrainingSet
  , model :: Model
  , motifs :: [Motif]
  , scanned_site_summary :: ScannedSiteSummary }
  deriving (Show, Eq)

-- | Training set defines the data used for the motif search
data TrainingSet = TrainingSet
  { trainingsetDatafile :: String
  , trainingsetLength :: Int
  , trainingsetAlphabet :: MemeAlphabet
  , trainingsetAmbigs :: MemeAmbigs
  , trainingsetSequences :: [Sequence]
  , trainingsetLetterFrequencies :: LetterFrequencies}
  deriving (Show, Eq)

data MemeAlphabet = MemeAlphabet
  { memeAlphabetId :: String
  , memeAlphabetLength :: Int
  , memeAlphabetLetters :: [MemeLetter]}
  deriving (Show, Eq)     
         
data MemeLetter = MemeLetter
  { memeLetterId :: String
  , memeLetterSymbol :: Char}
  deriving (Show, Eq)

data MemeAmbigs = MemeAmbigs
  { memeAmbigLetters :: [MemeLetter]}
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
           
-- The model describes how to training set was processed
data Model = Model
  { commandLine :: String
  , host :: String
  , modelType :: String
  , nmotifs :: Int
  , evalueThreshold :: String
  , objectFunction :: String
  , minWidth :: Int
  , maxWidth :: Int
  , minic :: Double
  , wg :: Int
  , ws :: Int
  , endGaps :: String
  , minSites :: Int
  , maxSites :: Int
  , wnSites :: Double
  , prob :: Int
  , spMap :: String
  , spFuzz :: Double
  , prior :: String
  , beta :: Double
  , maxiter :: Int
  , distance :: Double
  , numSequences :: Int
  , numPositions :: Int
  , seed :: Int
  , seqfrac :: String
  , strands :: String
  , priorsFile :: String
  , reasonForStopping :: String
  , backgroundFrequencies :: BackgroundFrequencies}
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

-- | The motifs are predicted by appying the model to the trainingset
data Motif = Motif
  { motifId :: String
  , motifName :: String
  , motifWidth :: Int
  , motifSites :: Int
  , motifIc :: Double
  , motifRe :: Double
  , motifLlr :: Int
  , motifEvalue :: Double
  , motifBayesTreshold :: Double
  , motifElapsedTime :: Double                          
  , motifRegularexpression :: String
  , motifScores :: Scores
  , motifProbabilites :: Propabilities                
  , motifContributingsites :: [ContributingSite]}
  deriving (Show, Eq)

data Scores = Scores
  { scoreAlphabetMatrix :: AlphabetMatrix}
  deriving (Show, Eq)

data Propabilities = Propabilities
  { propabilitiesAlphabetMatrix :: AlphabetMatrix}
  deriving (Show, Eq)

data AlphabetMatrix = AlphabetMatrix
  { alphabetMatrixArrays :: [AlphabetArray]}
  deriving (Show, Eq)        
  
data ContributingSite = ContributingSite
  { contributingSiteId :: String
  , contributingSitePosition :: Int
  , contributingSiteStrand :: String
  , contributingSitePvalue :: Double
  , contributingSiteLeftFlank :: String
  , contributingSiteRightFlank :: String
  , contributingSite :: Site}
  deriving (Show, Eq)

data Site = Site
  { siteLetterReferences :: [LetterReference]}
  deriving (Show, Eq)
             
data LetterReference = LetterReference
  { letterReference :: String}
  deriving (Show, Eq)

-- | Scanned sites contain all sites evaluated for possible motifs 
data ScannedSiteSummary = ScannedSiteSummary
  { p_thresh :: Double
  , scannedSites :: [ScannedSites]}
  deriving (Show, Eq)
           
data ScannedSites = ScannedSites
  { scannedsitesSequenceId :: String              
  , scannedSitesPvalue :: Double
  , numSites :: Int
  , scannedSiteArray :: [ScannedSite]}
  deriving (Show, Eq)
           
data ScannedSite = ScannedSite
  { scannedsiteMotifId :: String              
  , strand :: String
  , position :: Int
  , scannedSitePvalue :: Double}
  deriving (Show, Eq)
