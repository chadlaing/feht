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

  infoFile <- BS.readFile $ metafile userArgs
  dataFile <- BS.readFile $ datafile userArgs

  let parsedDataFile = parseDataFile delim dataFile
  --now we have all the information to fully populate the metadataInfo
  -- TODO: below messes up headers with space
  -- FIX: need to fix this (wrap in function)
  
  let metadataTable = getMetadataFromFile (nameColumnMap parsedDataFile) .
        fmap (BS.words. BS.unwords . BS.split delim) $ BS.lines infoFile
  --if we have SNP data, we need to convert it into binary first
  let finalDataTuples = convertDataToTuples (mode userArgs) delim (characterData parsedDataFile)
  let geneVectorMap = getGeneVectorMap finalDataTuples
  let cl = getComparisonList metadataTable groupCategories
  let resultMap = generateResultMap geneVectorMap cl

  let finalGroupComps = applyMultipleTestingCorrection (correction userArgs) resultMap
  let tableOfComps = formatComparisonResultsAsTable finalGroupComps
  mapM_ BS.putStrLn tableOfComps
  putStrLn "Done"
