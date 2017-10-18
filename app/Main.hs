{-# LANGUAGE OverloadedStrings #-}

import           Comparison
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Function
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

  infoFile <- BS.readFile $ metafile userArgs
  dataFile <- BS.readFile $ datafile userArgs

  let parsedDataFile = parseDataFile delim dataFile
  
  let metadataTable = getMetadataFromFile delim parsedDataFile infoFile
  --if we have SNP data, we need to convert it into binary first
  let finalDataTuples = convertDataToTuples (mode userArgs) parsedDataFile
  let geneVectorMap = getGeneVectorMap finalDataTuples
  print geneVectorMap
  let cl = getComparisonList metadataTable groupCategories
  let resultMap = generateResultMap geneVectorMap cl

  let finalGroupComps = applyMultipleTestingCorrection (correction userArgs) resultMap
  let tableOfComps = formatComparisonResultsAsTable finalGroupComps
  mapM_ BS.putStrLn tableOfComps
  putStrLn "Done"
