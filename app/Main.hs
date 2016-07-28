{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Prelude                    (error)
import           Comparison
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Maybe
import           Data.String
import           Data.Bool
import           System.Console.CmdArgs
import           System.IO
import           Table
import           Text.Show
--import Data.Char
import qualified Data.HashMap.Strict        as M
--import qualified Data.Vector.Unboxed        as V
import           Data.List                  (zip)


--for command line processing using cmdargs
data UserInput = UserInput
    {info      :: FilePath
    ,datafile  :: FilePath
    ,mode :: String
    ,delimiter :: String
    } deriving (Data, Typeable, Show, Eq)

--note, to use a tab character, one needs to enter a literal tab on the command
--line. Eg. delimiter="     "
--do this via Ctrl-V<tab>
userInput :: Mode (CmdArgs UserInput)
userInput =  cmdArgsMode UserInput
    {info = def &= help "File of genome metadata information"
    ,datafile = def &= help "File of binary or snp data"
    ,mode = def &= help "mode of program, either 'binary', or 'snp' "
    ,delimiter = "\t" &= help "Delimiter used for info and data files"
    }


main :: IO ()
main = do
    userArgs <- cmdArgsRun userInput
    print userArgs

    --let metadata = getListOfMetadata $ zip [1..] uMetadata
    let (delim:_) = delimiter userArgs
    let uMode = mode userArgs
    --let delim = '\t'
    --we want to split the strain information file into lines
    --and then send a list of words for processing
    infoFile <- BS.readFile $ info userArgs

    --before we assign any metadata info, we want the column positions for each
    --of the genomes, to do this we read in the data file next
    --read in the data table
    dataFile <- BS.readFile $ datafile userArgs
    let dataLines = BS.lines dataFile
    --we only need the headers of the data table to map the names to columns
    --the first column header is blank in the data table or not needed, as it
    --is assumed the first column is gene data
    let (genomeNames:genomeData) = dataLines
    let (_:splitGenomeNames) = BS.split delim genomeNames

    --create a hashMap of genomeName -> columnNumber
    let nameColumnHash = assignColumnNumbersToGenome (zip splitGenomeNames [1..])

    --now we have all the information to fully populate the metadataInfo
    let metadataTable = getMetadataFromFile nameColumnHash . fmap (BS.split delim) $ BS.lines infoFile

    --if we have SNP data, we need to convert it into binary first
    let finalGenomeData = case uMode of
                            "binary" -> genomeData
                            "snp" -> convertSnpToBinary delim genomeData
                            _ -> error "Incorrect mode given, requires `snp` or `binary`"

    let geneVectorMap = getGeneVectorMap delim finalGenomeData

    let testGroupOne = filterTable metadataTable (MetaCategory "SourceState") [MetaValue "AB"]
    let testGroupTwo = filterTable metadataTable (MetaCategory "SourceState") [MetaValue "ON"]
    let testComp = calculateFetFromComparison
                    Comparison{compGroup1 = testGroupOne
                              ,compGroup2 = testGroupTwo}
                    geneVectorMap
    let tableOfComps = formatFETResultHashAsTable testComp
    mapM_ BS.putStrLn tableOfComps

--     --filter the results by pvalue
--     --simple Bonferroni correction
--     let filteredGroupComps = filterComparisonsByPValue groupComps
--
--     let tableOfComps = formatComparisonAsTable filteredGroupComps
--     mapM_ BS.putStrLn tableOfComps

    putStrLn "Done"
