{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

--   Parse meme XML output.
--   xml parsing is done with the HXT libary.

module Bio.MemeXML ( parseXML
                   , atTag
                   , atId
                   , rstrip
                   , getSequences
                   , getMotif
                   , getContributingSite
                   , getSiteSequence
                   , module Bio.MemeData) where

import Bio.MemeData
import Text.XML.HXT.Core
import Data.Char (isSpace)    

parseXML :: String -> IOStateArrow s b XmlTree              
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

--atTag :: ArrowXml a => String -> a (XmlTree XNode) XmlTree
atTag :: ArrowXml a =>  String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)
--atId :: ArrowXml a => String -> a (Data.Tree.NTree.TypeDefs.NTree XNode) XmlTree
atId :: ArrowXml a =>  String -> a XmlTree XmlTree
atId elementId = deep (isElem >>> hasAttrValue "id" (== elementId) )
--remove whitespaces
rstrip :: [Char] -> [Char]
rstrip = reverse . dropWhile isSpace . reverse

getSequences :: ArrowXml a => a XmlTree Sequence          
getSequences = atTag "sequence" >>> 
  proc nucleotideSequence -> do
    nucleotideSeqId <- getAttrValue "id" -< nucleotideSequence
    nucleotideSeqName <- getAttrValue "name" -< nucleotideSequence
    nucleotideSeqLength <- getAttrValue "length" -< nucleotideSequence
    nucleotideSeqWeight <- getAttrValue "weight" -< nucleotideSequence
    returnA -< Sequence {
      sequenceId = nucleotideSeqId, 
      sequenceName = nucleotideSeqName,
      sequenceLength = read (nucleotideSeqLength) :: Int,
      sequenceWeight = read (nucleotideSeqWeight) :: Double}

getMotif :: ArrowXml a => a XmlTree Motif  
getMotif = atTag "motif"  >>> 
  proc motif -> do
    id_name <- getAttrValue "id" -< motif
    motif_width <- getAttrValue "width" -< motif
    regex <- getText <<< getChildren <<<  atTag "regular_expression" -< motif
    contributingsites <- listA getContributingSite -< motif
    returnA -< Motif {
      motifId = rstrip id_name, 
      motifWidth = rstrip motif_width,
      -- regex field contains 2 linebreaks
      motifRegularexpression = filter (/= '\n') regex,
      motifContributingsites = contributingsites }

getContributingSite :: ArrowXml a => a XmlTree ContributingSite  
getContributingSite = atTag "contributing_site" >>> 
  proc contributingsite -> do
  contributing_site_id <- getAttrValue "sequence_id" -< contributingsite
  contributing_site_position <- getAttrValue "position" -< contributingsite
  contributing_site_strand <- getAttrValue "strand" -< contributingsite
  contributing_site_pvalue <- getAttrValue "pvalue" -< contributingsite
  contributing_site_sequence <- listA getSiteSequence -< contributingsite
  returnA -<  ContributingSite {
    contributingSiteId = contributing_site_id,
    contributingSitePosition = contributing_site_position,
    contributingSiteStrand = contributing_site_strand,
    contributingSitePvalue = contributing_site_pvalue,
    contributingSiteSequence = contributing_site_sequence}

getSiteSequence :: ArrowXml a => a XmlTree LetterReference
getSiteSequence = atTag "letter_ref" >>>
   proc letterreference -> do
   letterid <- getAttrValue "letter_id" -< letterreference
   returnA -< LetterReference {
     letterReference = letterid }

   
