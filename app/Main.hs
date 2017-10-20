{-# LANGUAGE OverloadedStrings #-}

import           Comparison
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Function
import           Options.Applicative
import           Prelude                    ()
import Data.Int
import Data.List
import           System.IO
import           Table
import           UserInput
import Data.HashMap.Strict as M

getLength :: [Int]
          -> [ComparisonResult]
          -> [Int]
getLength zs z = (length z):zs

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

  let rl = M.foldl' getLength [] resultMap
  print rl

  let finalGroupComps =
        applyMultipleTestingCorrection (correction userArgs) resultMap
  let fgl = M.foldl' getLength [] finalGroupComps
  print fgl
  
  let tableOfComps = formatComparisonResultsAsTable (ratioFilter userArgs) finalGroupComps
  mapM_ BS.putStrLn tableOfComps
  putStrLn "Done"

  
