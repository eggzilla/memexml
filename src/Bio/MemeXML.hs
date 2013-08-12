{- | Parse meme XML output.
   xml parsing is done with the HXT libary.
-}


{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Bio.MemeXML ( parseXML
                    , module Bio.MemeData) where

import System.Environment (getArgs) 
import Text.XML.HXT.Core 
import System.IO
import System.Console.GetOpt
import Data.Char (isSpace)    
import Bio.MemeData
              
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file

atTag tag = deep (isElem >>> hasName tag) 
atId id = deep (isElem >>> hasAttrValue "id" (== id) )
--remove whitespaces
rstrip = reverse . dropWhile isSpace . reverse
--hasID id xs= filter (\id -> sequenceId == id) xs 
          
getSequences = atTag "sequence" >>> 
  proc sequence -> do
    seqid <- getAttrValue "id" -< sequence
    seqname <- getAttrValue "name" -< sequence
    seqlength <- getAttrValue "length" -< sequence
    returnA -< Sequence {
      sequenceId = seqid, 
      sequenceName = seqname,
      sequenceLength = seqlength}
    
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

getContributingSite = atTag "contributing_site" >>> 
  proc contributingsite -> do
  contributing_site_id <- getAttrValue "sequence_id" -< contributingsite
  contributing_site_position <- getAttrValue "position" -< contributingsite
  contributing_site_strand <- getAttrValue "strand" -< contributingsite
  contributing_site_pvalue <- getAttrValue "pvalue" -< contributingsite
  contributing_site_sequence <- listA getSiteSequence -< contributingsite
  returnA -<  ContributingSite {
    siteId = contributing_site_id,
    sitePosition = contributing_site_position,
    siteStrand = contributing_site_strand,
    sitePvalue = contributing_site_pvalue,
    siteSequence = contributing_site_sequence}

getSiteSequence = atTag "letter_ref" >>>
   proc letterreference -> do
   letterid <- getAttrValue "letter_id" -< letterreference
   returnA -< LetterReference {
     letterReference = letterid }

   
