{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

import           Prelude                    ()
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
    ,mone :: String
    ,mtwo   :: String
    ,mthree :: String
    ,mfour :: String
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
    ,mone = def &= help "List of space-delimited and quoted metadata types for category one"
    ,mtwo = def &= help "List of space-delimited and quoted metadata types for category two"
    ,mthree = def &= help "List of space-delimited and quoted metadata types for category three"
    ,mfour = def &= help "List of space-delimited and quoted metadata types for category four"
    ,delimiter = "," &= help "Delimiter used for info and data files"
    }


main :: IO ()
main = do
    userArgs <- cmdArgsRun userInput
    print userArgs

    --the input of metadata is space separated
    --we need to convert them into "Serotype, Location, Source" etc.  for use in the program
    --let uSerotype = fmap (Serotype  BS.pack) . words . serotype $ userArgs
    let uMetadata = [mone userArgs
                    ,mtwo userArgs
                    ,mthree userArgs
                    ,mfour userArgs
                    ]

    let metadata = getListOfMetadata $ zip [1..] uMetadata
    let (delim:_) = delimiter userArgs
    let uMode = mode userArgs
    --let delim = '\t'
    --we want to split the strain information file into lines
    --and then send a list of words for processing
    infoFile <- BS.readFile $ info userArgs
    let metadataInfo = getMetadataFromFile . fmap (BS.split delim) $ BS.lines infoFile

    -- --read in Table header
    -- --add the column number to the Table for each genome
    dataFile <- BS.readFile $ datafile userArgs
    let dataLines = BS.lines dataFile
    --we only need the headers of the data table to map the names to columns
    let (genomeNames:genomeData) = dataLines

    --split the header line on the delimiter to get all genome names
    --add the column number to each metadata table
    --remove entries not present in the column headers
    let finalMetadataInfo = addColumnNumbers metadataInfo (BS.split delim genomeNames)
    --print finalMetadataInfo
    --the strain information sheet contains information on more genomes than
    --exist in the data sheet
    --filter out the genomes for which there are no data
    let filteredMetadataInfo = M.filter (isJust . unColumnNumber . columnNumber) finalMetadataInfo

    --all but the header information is used for generating the
    --map of geneName --> dataVector

    --if we have SNP data, we need to convert it into binary first
    let finalGenomeData = case uMode == "binary" of
                            True -> genomeData
                            False -> convertSnpToBinary delim genomeData

    let geneVectorMap = getGeneVectorMap delim finalGenomeData




    -- print geneVectorMap
    -- let serotypeTable = countByMetadata serotype filteredInfoTable
    -- let summaryTable = summaryMetadataTable serotypeTable
    -- mapM_ putStrLn summaryTable

    let groupComps = calculateMetadata filteredMetadataInfo geneVectorMap metadata
    print groupComps
--
    --filter the results by pvalue
    --simple Bonferroni correction
    let filteredGroupComps = filterComparisonsByPValue groupComps

    let tableOfComps = formatComparisonAsTable filteredGroupComps
    mapM_ BS.putStrLn tableOfComps

    putStrLn "Done"
