{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Comparison
import           Control.Applicative
import           Control.Monad
import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Eq
import           Data.Function
import           Data.Functor
import qualified Data.HashMap.Strict        as M
import           Data.List
import           Data.Maybe
import           Data.Semigroup             ((<>))
import           Data.String
import           Options.Applicative
import           Prelude                    (error, Char)
import           System.IO
import           Table
import           Text.Show


--for command line processing using cmdargs
data UserInput = UserInput
    {metafile   :: FilePath
    ,datafile   :: FilePath
    ,one       :: String
    ,two       :: String
    ,delimiter  :: String
    ,mode       :: String
    ,correction :: String
    } deriving (Show, Eq)



feht :: Parser UserInput
feht = UserInput
  <$> strOption
      (long "metafile"
      <> short 'm'
      <> metavar "FILE"
      <> help "File of metadata information")
  <*> strOption
      (long "datafile"
      <> short 'd'
      <> metavar "FILE"
      <> help "File of binary or single-nucleotide variant data")
  <*> strOption
      (long "one"
      <> metavar "Group1Name Group1Item Group1Item Group1Item"
      <> help "Group1 column name, followed by optional Group1 labels to include as part of the group")
  <*> strOption
      (long "two"
      <> metavar "Group2Name Group2Item Group2Item Group2Item"
      <> help "Group2 column name, followed by optional Group2 labels to include as part of the group")
  <*> strOption
      (long "delimiter"
      <> short 'l'
      <> metavar "[',', '\\t' ...], DEFAULT=','"
      <> value ","
      <> help "Delimiter used for both the metadata and data file")
  <*> strOption
      (long "mode"
      <> short 'o'
      <> metavar "['binary', 'snp'], DEFAULT='binary'"
      <> value "binary"
      <> help "Mode for program data; either 'binary' or 'snp'")
  <*> strOption
      (long "correction"
      <> short 'c'
      <> metavar "['none', 'bonferroni'], DEFAULT='bonferroni'"
      <> value "bonferroni"
      <> help "Multiple-testing correction to apply"
      )

opts :: ParserInfo UserInput
opts = info (feht <**> helper)
  (fullDesc
  <> progDesc "Predictive marker discovery for groups; binary data, genomic data (single nucleotide variants), and arbitrary character data."
  <> header "feht - predictive marker discovery")

main :: IO ()
main = do
  userArgs <- execParser opts
  let (delim:_) = delimiter userArgs
  let uMode = mode userArgs
  print userArgs

  let onexs = words $ one userArgs
  let twoxs = words $ two userArgs
  let groupOneCategory = MetaCategory $ BS.pack $ head onexs
  let groupTwoCategory = MetaCategory $ BS.pack $ head twoxs
  let groupOneValues = (MetaValue . BS.pack) <$> tail onexs
  let groupTwoValues = (MetaValue . BS.pack) <$> tail twoxs

  print groupOneCategory
  print groupOneValues
  print groupTwoCategory
  print groupTwoValues

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
  print genomeNames
  let splitGenomeNames = (BS.words . BS.unwords) $ BS.split delim genomeNames
  print splitGenomeNames
  --create a hashMap of genomeName -> columnNumber
  let nameColumnHash = assignColumnNumbersToGenome (zip splitGenomeNames [1..])
  print nameColumnHash

  --now we have all the information to fully populate the metadataInfo
  let metadataTable = getMetadataFromFile nameColumnHash . fmap (BS.words. BS.unwords . BS.split delim) $ BS.lines infoFile
  print metadataTable

  --if we have SNP data, we need to convert it into binary first
  let finalGenomeData = case mode userArgs of
                          "binary" -> binaryDataToTuples delim genomeData
                          "snp" -> convertSnpToBinary delim genomeData
                          _ -> error "Incorrect mode given, requires `snp` or `binary`"


  let geneVectorMap = getGeneVectorMap finalGenomeData
  let cl = getComparisonList metadataTable (groupOneCategory, groupOneValues) (groupTwoCategory, groupTwoValues)
  print cl
--
  let resultMap = generateResultMap geneVectorMap cl
--
  --filter the results by pvalue if selected
  --simple Bonferroni correction
  let finalGroupComps = case correction userArgs of
                          "bonferroni" -> fmap filterResultsByPValue resultMap
                          "none" -> resultMap
                          _ -> error "Incorrect multiple testing correction supplied"
-- -
--  let tableOfComps = concatMap formatFETResultHashAsTable finalGroupComps
  -- mapM_ BS.putStrLn tableOfComps
  print resultMap
  putStrLn "Done"
