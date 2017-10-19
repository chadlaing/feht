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
  let groupCategories = generateCategories (one userArgs) (two userArgs)

  infoFile <- BS.readFile $ metafile userArgs
  dataFile <- BS.readFile $ datafile userArgs

  let parsedDataFile =
        parseDataFile (delimiter userArgs) (mode userArgs) dataFile
  let metadataTable =
        getMetadataFromFile (delimiter userArgs) parsedDataFile infoFile

  let cl = getComparisonList metadataTable groupCategories
  let resultMap = generateResultMap (geneVectorMap parsedDataFile) cl
  let finalGroupComps =
        applyMultipleTestingCorrection (correction userArgs) resultMap
  let tableOfComps = formatComparisonResultsAsTable finalGroupComps
  mapM_ BS.putStrLn tableOfComps
  putStrLn "Done"
