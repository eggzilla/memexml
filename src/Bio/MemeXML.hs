{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

--   Parse Multiple EM for Motif Elicitation (MEME) XML output.
--   xml parsing is done with the HXT libary.

module Bio.MemeXML ( parseXML
                   , atTag
                   , atId
                   , rstrip
                   , getSequence
                   , getMotif
                   , getContributingSite
                   , module Bio.MemeData) where

import Bio.MemeData
import Text.XML.HXT.Core
import Data.Char (isSpace)    

parseXML :: String -> IOStateArrow s b XmlTree              
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file


atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)
            
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId))
                 
rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse


getMemeResult :: ArrowXml a => a XmlTree MemeResult  
getMemeResult = atTag "MEME" >>> 
  proc memeResult -> do
  memeResult_version <- getAttrValue "version" -< memeResult
  memeResult_release <- getAttrValue "release" -< memeResult
  memeResult_training_set  <- getTrainingSet -< memeResult
  memeResult_model  <- getModel -< memeResult
  memeResult_motifs  <- listA getMotif -< memeResult
  memeResult_scanned_site_summary  <- getScannedSiteSummary -< memeResult
  returnA -< MemeResult {
    memeversion = memeResult_version,
    memerelease = memeResult_release,
    trainingset = memeResult_training_set,
    model = memeResult_model,
    motifs = memeResult_motifs,
    scanned_site_summary = memeResult_scanned_site_summary}

getTrainingSet :: ArrowXml a => a XmlTree TrainingSet  
getTrainingSet = atTag "training_set" >>> 
  proc trainingSet -> do
  trainingSet_datafile <- getAttrValue "datafile" -< trainingSet
  trainingSet_length <- getAttrValue "length" -< trainingSet
  trainingSet_alphabet <- getMemeAlphabet -< trainingSet
  trainingSet_ambigs <- getAmbigs -< trainingSet
  trainingSet_sequences <- listA getSequence -< trainingSet
  trainingSet_letterfrequencies <- getLetterFrequencies -< trainingSet
  returnA -< TrainingSet {
    trainingsetDatafile = trainingSet_datafile,
    trainingsetLength = read (trainingSet_length) :: Int,
    trainingsetAlphabet = trainingSet_alphabet,
    trainingsetAmbigs = trainingSet_ambigs,
    trainingsetSequences = trainingSet_sequences,
    trainingsetLetterFrequencies = trainingSet_letterfrequencies}

--gettrainingset
  
  --getalphabet
getMemeAlphabet :: ArrowXml a => a XmlTree MemeAlphabet
getMemeAlphabet = atTag "alphabet" >>> 
  proc memealphabet -> do
    alphabet_id <- getAttrValue "id" -< memealphabet
    alphabet_length <- getAttrValue "length" -< memealphabet
    alphabet_letters <- listA getLetter -< memealphabet
    returnA -< MemeAlphabet {
      memeAlphabetId = alphabet_id, 
      memeAlphabetLength = read (alphabet_length) :: Int,
      memeAlphabetLetters = alphabet_letters}

getLetter :: ArrowXml a => a XmlTree MemeLetter
getLetter = atTag "letter" >>> 
  proc memeletter -> do
    letter_id <- getAttrValue "id" -< memeletter
    letter_symbol <- getAttrValue "symbol" -< memeletter
    returnA -< MemeLetter {
      memeLetterId = letter_id, 
      memeLetterSymbol = read (letter_symbol) :: Char}

  --getambigs
getAmbigs :: ArrowXml a => a XmlTree MemeAmbigs
getAmbigs = atTag "ambigs" >>> 
  proc memeambigs -> do
    ambigs_letters <- listA getLetter -< memeambigs
    returnA -< MemeAmbigs {
      memeAmbigLetters = ambigs_letters} 

getLetterFrequencies :: ArrowXml a => a XmlTree LetterFrequencies
getLetterFrequencies = atTag "letter_frequencies" >>> 
  proc memeletterfrequencies -> do
    letterfrequencies_alphabetarray <- getAlphabetArray -< memeletterfrequencies
    returnA -< LetterFrequencies {
      letterFrequenciesAlphabetArray = letterfrequencies_alphabetarray} 
  
getAlphabetArray :: ArrowXml a => a XmlTree AlphabetArray
getAlphabetArray =  atTag "alphabet_array" >>> 
  proc memealphabetarray -> do
    alphabetarray_values <- listA getAlphabetArrayValues -< memealphabetarray
    returnA -< AlphabetArray {
      alphabetArrayValues = alphabetarray_values} 

getAlphabetArrayValues :: ArrowXml a => a XmlTree AlphabetArrayValue
getAlphabetArrayValues =  atTag "alphabet_array_value" >>> 
  proc memealphabetarrayvalue -> do
    alphabetarrayvalue_id <- getAttrValue "letter_id" -< memealphabetarrayvalue
    alphabetarrayvalue_frequency <- getText <<< getChildren -< memealphabetarrayvalue
    returnA -< AlphabetArrayValue {
      letterId = alphabetarrayvalue_id,
      frequency = read (alphabetarrayvalue_frequency) :: Double} 

getModel :: ArrowXml a => a XmlTree Model
getModel = atTag "model" >>>
  proc mememodel -> do
     model_command_line <- getText <<< getChildren <<< atTag "command_line" -< mememodel
     model_host <- getText <<< getChildren <<< atTag "host" -< mememodel
     model_type <- getText <<< getChildren <<< atTag "type" -< mememodel
     model_nmotifs <- getText <<< getChildren <<< atTag "nmotifs" -< mememodel
     model_evalue_threshold <- getText <<< getChildren <<< atTag "evalue_threshold" -< mememodel
     model_object_function <- getText <<< getChildren <<< atTag "object_function" -< mememodel
     model_min_width <- getText <<< getChildren <<< atTag "min_width" -< mememodel
     model_max_width <- getText <<< getChildren <<< atTag "max_width" -< mememodel
     model_minic <- getText <<< getChildren <<< atTag "minic" -< mememodel
     model_wg <- getText <<< getChildren <<< atTag "wg" -< mememodel
     model_ws <- getText <<< getChildren <<< atTag "ws"-< mememodel
     model_endgaps <- getText <<< getChildren <<< atTag "endgaps" -< mememodel
     model_minsites <- getText <<< getChildren <<< atTag "minsites" -< mememodel
     model_maxsites <- getText <<< getChildren <<< atTag "maxsites" -< mememodel
     model_wnsites <- getText <<< getChildren <<< atTag "wnsites" -< mememodel
     model_prob <- getText <<< getChildren <<< atTag "prob" -< mememodel
     model_spmap <- getText <<< getChildren <<< atTag "spmap" -< mememodel
     model_spfuzz <- getText <<< getChildren <<< atTag "spfuzz" -< mememodel
     model_prior <- getText <<< getChildren <<< atTag "prior"-< mememodel
     model_beta <- getText <<< getChildren <<< atTag "beta" -< mememodel
     model_maxiter <- getText <<< getChildren <<< atTag "maxiter" -< mememodel
     model_distance <- getText <<< getChildren <<< atTag "distance" -< mememodel
     model_num_sequences <- getText <<< getChildren <<< atTag "num_sequences" -< mememodel
     model_num_positions <- getText <<< getChildren <<< atTag "num_positions" -< mememodel
     model_seed <- getText <<< getChildren <<< atTag "seed" -< mememodel
     model_seqfrac <- getText <<< getChildren <<< atTag "seqfrac" -< mememodel
     model_strands <- getText <<< getChildren <<< atTag "strands" -< mememodel
     model_priors_file <- getText <<< getChildren <<< atTag "priors_file" -< mememodel
     model_reason_for_stopping <- getText <<< getChildren <<< atTag "reason_for_stopping"-< mememodel
     model_background_frequencies <- getBackgroundFrequencies -< mememodel
     returnA -< Model {
        commandLine = model_command_line,
        host = model_host,
        modelType = model_type,
        nmotifs = read (model_nmotifs) :: Int,
        evalueThreshold = model_evalue_threshold,
        objectFunction = model_object_function,
        minWidth = read (model_min_width) :: Int,
        maxWidth = read (model_max_width) :: Int,
        minic = read (model_minic) :: Double,
        wg = read (model_wg) :: Int,
        ws = read (model_ws) :: Int,
        endGaps = model_endgaps,
        minSites = read (model_minsites) :: Int,
        maxSites = read (model_maxsites) :: Int,
        wnSites = read (model_wnsites) :: Double,
        prob = read (model_prob) :: Int,
        spMap = model_spmap,
        spFuzz = read (model_spfuzz) :: Double,
        prior = model_prior,
        beta = read (model_beta) :: Double,
        maxiter = read (model_maxiter) :: Int,
        distance = read (model_distance) :: Double,
        numSequences =  read (model_num_sequences) :: Int,
        numPositions = read (model_num_positions) :: Int,
        seed = read (model_seed) :: Int,
        seqfrac = model_seqfrac,
        strands =  model_strands,
        priorsFile =  model_priors_file,
        reasonForStopping =  model_reason_for_stopping,
        backgroundFrequencies = model_background_frequencies}

getBackgroundFrequencies :: ArrowXml a => a XmlTree BackgroundFrequencies
getBackgroundFrequencies =  atTag "background_frequencies" >>> 
  proc memebackgroundfrequencies -> do
    backgroundfrequencies_source <- getAttrValue "source" -< memebackgroundfrequencies
    backgroundfrequencies_alphabetarray <- getAlphabetArray -<  memebackgroundfrequencies
    returnA -< BackgroundFrequencies {
      source = backgroundfrequencies_source,
      backgroundFrequenciesAlphabetArray = backgroundfrequencies_alphabetarray} 

getSequence :: ArrowXml a => a XmlTree Sequence          
getSequence = atTag "sequence" >>> 
  proc nucleotideSequence -> do
    nucleotide_SeqId <- getAttrValue "id" -< nucleotideSequence
    nucleotide_SeqName <- getAttrValue "name" -< nucleotideSequence
    nucleotide_SeqLength <- getAttrValue "length" -< nucleotideSequence
    nucleotide_SeqWeight <- getAttrValue "weight" -< nucleotideSequence
    returnA -< Sequence {
      sequenceId = nucleotide_SeqId, 
      sequenceName = nucleotide_SeqName,
      sequenceLength = read (nucleotide_SeqLength) :: Int,
      sequenceWeight = read (nucleotide_SeqWeight) :: Double}
-----


getMotif :: ArrowXml a => a XmlTree Motif  
getMotif = atTag "motif"  >>> 
  proc mememotif -> do
    motif_id <- getAttrValue "id" -< mememotif
    motif_name <- getAttrValue "name" -< mememotif
    motif_width <- getAttrValue "width" -< mememotif
    motif_sites <- getAttrValue "sites" -< mememotif
    motif_ic <- getAttrValue "ic" -< mememotif
    motif_re <- getAttrValue "re" -< mememotif
    motif_llr <- getAttrValue "llr" -< mememotif
    motif_e_value <- getAttrValue "e_value" -< mememotif
    motif_bayes_threshold <- getAttrValue "bayes_threshold" -< mememotif
    motif_elapsed_time <- getAttrValue "elapsed_time" -< mememotif
    motif_scores <- getScores -< mememotif
    motif_propabilities <- getPropabilities -< mememotif
    regex <- getText <<< getChildren <<<  atTag "regular_expression" -< mememotif
    contributingsites <- listA getContributingSite -< mememotif
    returnA -< Motif {
      motifId = motif_id, 
      motifName = motif_name,
      motifWidth = read (motif_width) :: Int,
      motifSites = read (motif_sites) :: Int,
      motifIc = read (motif_ic) :: Double,
      motifRe = read (motif_re) :: Double,
      motifLlr = read (motif_llr) :: Int, 
      motifEvalue = read (motif_e_value) :: Double,
      motifBayesTreshold = read (motif_bayes_threshold) :: Double,
      motifElapsedTime = read (motif_elapsed_time) :: Double,
      -- regex field contains 2 linebreaks
      motifRegularexpression = filter (/= '\n') regex,
      motifScores = motif_scores,
      motifProbabilites = motif_propabilities,
      motifContributingsites = contributingsites }

getScores :: ArrowXml a => a XmlTree Scores
getScores = atTag "scores" >>>
  proc memescores -> do
  scores_alphabetmatrix <-getAlphabetMatrix  -< memescores
  returnA -< Scores {
    scoreAlphabetMatrix = scores_alphabetmatrix}

getPropabilities :: ArrowXml a => a XmlTree Propabilities
getPropabilities = atTag "propabilities" >>>
  proc memepropabilities -> do
  propabilities_alphabetmatrix <-getAlphabetMatrix  -< memepropabilities
  returnA -< Propabilities {
    propabilitiesAlphabetMatrix = propabilities_alphabetmatrix}

getAlphabetMatrix :: ArrowXml a => a XmlTree AlphabetMatrix
getAlphabetMatrix = atTag "alphabet_matrix" >>>
  proc memeaalphabetmatrix -> do
    alphabet_matrix <- listA getAlphabetArray -< memeaalphabetmatrix
    returnA -< AlphabetMatrix {
      alphabetMatrixArrays = alphabet_matrix} 
   
getContributingSite :: ArrowXml a => a XmlTree ContributingSite  
getContributingSite = atTag "contributing_site" >>> 
  proc contributingsite -> do
  contributing_site_id <- getAttrValue "sequence_id" -< contributingsite
  contributing_site_position <- getAttrValue "position" -< contributingsite
  contributing_site_strand <- getAttrValue "strand" -< contributingsite
  contributing_site_pvalue <- getAttrValue "pvalue" -< contributingsite
  contributing_site_left_flank <- getText <<< getChildren <<< atTag "left_flank"  -< contributingsite
  contributing_site <- getSite  -< contributingsite
  contributing_site_right_flank <- getText <<< getChildren <<< atTag "right_flank"  -< contributingsite
  returnA -<  ContributingSite {
    contributingSiteId = contributing_site_id,
    contributingSitePosition = read (contributing_site_position) :: Int,
    contributingSiteStrand = contributing_site_strand,
    contributingSitePvalue = read (contributing_site_pvalue) :: Double,
    contributingSiteLeftFlank = contributing_site_left_flank,
    contributingSite = contributing_site,
    contributingSiteRightFlank = contributing_site_right_flank}

getSite :: ArrowXml a => a XmlTree Site
getSite = atTag "site" >>>
   proc site -> do
   site_letterreferences <- listA getSiteLetterReference -< site
   returnA -< Site {
     siteLetterReferences = site_letterreferences}
 
getSiteLetterReference :: ArrowXml a => a XmlTree LetterReference
getSiteLetterReference = atTag "letter_ref" >>>
  proc letterreference -> do
  letterid <- getAttrValue "letter_id" -< letterreference
  returnA -< LetterReference {
    letterReference = letterid}
   
getScannedSiteSummary :: ArrowXml a => a XmlTree ScannedSiteSummary
getScannedSiteSummary = atTag "scanned_sites_summary" >>>
  proc scannedsitesummary  -> do
    scannedsitesummary_p_tresh <- getAttrValue "p_thresh" -< scannedsitesummary
    scannedsites <- listA getScannedSites -< scannedsitesummary
    returnA -< ScannedSiteSummary{
      p_thresh = read (scannedsitesummary_p_tresh) :: Double,
      scannedSites = scannedsites}

getScannedSites :: ArrowXml a => a XmlTree ScannedSites
getScannedSites = atTag "scanned_sites" >>>
  proc scannedsites  -> do
    scannedsites_sequence_id <- getAttrValue "sequence_id" -< scannedsites
    scannedsites_pvalue <- getAttrValue "pvalue" -< scannedsites
    scannedsites_num_sites <- getAttrValue "num_sites" -< scannedsites       
    scannedsites_array <- listA getScannedSite -< scannedsites
    returnA -< ScannedSites{
      scannedsitesSequenceId = scannedsites_sequence_id,
      scannedSitesPvalue = read (scannedsites_pvalue) :: Double,
      numSites = read (scannedsites_num_sites) :: Int,
      scannedSiteArray = scannedsites_array}
                               
getScannedSite :: ArrowXml a => a XmlTree ScannedSite
getScannedSite = atTag "scanned_site" >>>
  proc scannedsite  -> do
    scannedsite_motif_id <- getAttrValue "motif_id" -< scannedsite
    scannedsite_strand <- getAttrValue "strand" -< scannedsite
    scannedsite_position <- getAttrValue "position" -< scannedsite     
    scannedsite_sitepvalue <- getAttrValue "pvalue" -< scannedsite
    returnA -< ScannedSite{
      scannedsiteMotifId = scannedsite_motif_id,
      strand = scannedsite_strand,
      position = read (scannedsite_position) :: Int,
      scannedSitePvalue = read (scannedsite_sitepvalue) :: Double}   
