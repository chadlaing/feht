{-# LANGUAGE OverloadedStrings #-}

import           Comparison
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Function
import           Data.List
import           Options.Applicative
import           Prelude                    ()
import           System.IO
import           Table
import           UserInput

main :: IO ()
main = do
  userArgs <- execParser opts

  let delim = delimiter userArgs
  let groupCategories = generateCategories (one userArgs) (two userArgs)

  --we want to split the strain information file into lines
  --and then send a list of words for processing
  infoFile <- BS.readFile $ metafile userArgs

  --before we assign any metadata info, we want the column positions for each
  --of the genomes, to do this we read in the data file next
  dataFile <- BS.readFile $ datafile userArgs
  let dataLines = BS.lines dataFile

  --we only need the headers of the data table to map the names to columns
  --the first column header is blank in the data table or not needed, as it
  --is assumed the first column is gene data
  let (genomeNames:genomeData) = dataLines
  let splitGenomeNames = (BS.words . BS.unwords) $ BS.split delim genomeNames
  --create a hashMap of genomeName -> columnNumber
  let nameColumnHash = assignColumnNumbersToGenome (zip splitGenomeNames [1..])

  --now we have all the information to fully populate the metadataInfo
  let metadataTable = getMetadataFromFile nameColumnHash . fmap (BS.words. BS.unwords . BS.split delim) $ BS.lines infoFile

  --if we have SNP data, we need to convert it into binary first
  let finalDataTuples = convertDataToTuples (mode userArgs) delim genomeData

  let geneVectorMap = getGeneVectorMap finalDataTuples
  let cl = getComparisonList metadataTable groupCategories
  let resultMap = generateResultMap geneVectorMap cl

  let finalGroupComps = applyMultipleTestingCorrection (correction userArgs) resultMap
  let tableOfComps = formatComparisonResultsAsTable finalGroupComps
  mapM_ BS.putStrLn tableOfComps
  putStrLn "Done"
