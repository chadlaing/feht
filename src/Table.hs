{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Table where

import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Eq
import           Data.Foldable
import           Data.Function
import           Data.Functor
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics               (Generic)
import           Prelude                    (String, error, words, (-))
import           Safe
import           Text.Read
import           Text.Show
import           UserInput



newtype GeneName = GeneName { unGeneName :: BS.ByteString } deriving (Eq, Show, Ord, Generic)
newtype MetaCategory = MetaCategory {unMetaCategory :: BS.ByteString} deriving (Eq, Show, Ord, Generic)
newtype MetaValue = MetaValue {unMetaValue :: BS.ByteString} deriving (Eq, Show, Ord, Generic)

data GenomeInfo =
    GenomeInfo{genomeName   :: BS.ByteString
              ,columnNumber :: Int} deriving (Show, Eq, Ord, Generic)


data FilterType = FilterCategory | AllButCategory deriving (Ord, Read, Show, Eq)

-- |Store the categories and associated values for the analyses
data GroupCategories =
  MkGroupCategories
  {oneCategory :: MetaCategory
  ,oneValues   :: [MetaValue]
  ,twoCategory :: MetaCategory
  ,twoValues   :: [MetaValue]} deriving(Eq, Show)


type GeneVectorMap = M.HashMap GeneName (V.Vector Char)
data ParsedDataFile =
  MkParsedDataFile
  {nameColumnMap :: GenomeNameColumnHash
  ,geneVectorMap :: GeneVectorMap
  }deriving (Eq, Show)


--make classes instances of Hashable
--this requires the import of GHC.Generics (Generic) and Data.Hashable
--also requires the Type to derive Generic
--then a new instance for Hashable can be created
instance Hashable GeneName
instance Hashable GenomeInfo
instance Hashable MetaCategory
instance Hashable MetaValue


type MetaHash = M.HashMap MetaCategory MetaValue
type Table = M.HashMap GenomeInfo MetaHash
type MetaCats = [BS.ByteString]


-- |For each line in the file, except the first one, we want to create a record
-- with all of the information given in Table
-- the first one contains column headers
-- The first line will be used to get the MetaCategory items
-- The first column of the metadata file needs no header
-- as this is the row labels for the data. Thus metaCats takes the tail of the
-- first row.
getMetadataFromFile :: Char
                    -> ParsedDataFile
                    -> BS.ByteString
                    -> Table
getMetadataFromFile _ _ (BS.uncons -> Nothing) = error "File is empty"
getMetadataFromFile delim pd x = foldl' (getEntry metaCats pd) M.empty splitMetaData
  where
    metaCats = tailNote "No metadata categories" $ BS.split delim metaHeader
    xs = BS.filter ('\r' /=) <$> BS.lines x
    splitMetaData = BS.split delim <$> metaData
    (metaHeader, metaData) = fromMaybe (error "Metadata file missing header information, data, or both") (getHeadAndTail xs)

-- |Insert the metaHash as a value into the GenomeInfo key
-- the metahash is built by folding metaAssign over the data
getEntry :: MetaCats
         -> ParsedDataFile
         -> Table
         -> [BS.ByteString]
         -> Table
getEntry _ _ _ []  = error "Cannot get entry from metadata file"
getEntry mc pd t xs = M.insert gi metaHash t
  where
    gi = GenomeInfo gName cNum
    (gName, xs') = fromMaybe (error "Metadata line missing name, data, or both") (getHeadAndTail xs)
    cNum = fromMaybe (error "name was not found in the data file") (M.lookup gName (nameColumnMap pd))
    metaHash = foldl' metaAssign M.empty alignedMeta
    alignedMeta = zip mc xs'


metaAssign :: MetaHash
           -> (BS.ByteString, BS.ByteString)
           -> MetaHash
metaAssign mh (category, value) =
  M.insert (MetaCategory category) (MetaValue value) mh
    --this will create a list of ByteString tuples, that need to be made
    -- into the appropriate MetaCategory and values







-- |We want a non-empty head and non-empty tail, otherwise give Nothing
getHeadAndTail :: [a]
               -> Maybe (a,[a])
getHeadAndTail []     = Nothing
getHeadAndTail [_] = Nothing
getHeadAndTail (x:xs) = Just (x,xs)


-- getMetadataFromFile _ ([]:_) = error "File is empty"
-- getMetadataFromFile gnch ((_:header):xs) = foldl' getEntry M.empty xs
--   where

type GenomeNameColumnHash = M.HashMap BS.ByteString Int
assignColumnNumbersToGenome :: [(BS.ByteString, Int)]
                            -> GenomeNameColumnHash
assignColumnNumbersToGenome = foldl' assignColumn M.empty
  where
    assignColumn :: GenomeNameColumnHash
                 -> (BS.ByteString, Int)
                 -> GenomeNameColumnHash
    assignColumn g (n, i) = M.insert n i g


type BinaryTuple = (BS.ByteString, BS.ByteString)

convertDataToTuples :: UserMode
                    -> [[BS.ByteString]]
                    -> [BinaryTuple]
convertDataToTuples m cd
  | m == Snp = convertSnpToBinary cd
  | otherwise = binaryDataToTuples cd


-- |We are looking to create tuples of the geneID / values
-- split on the delimiter
binaryDataToTuples :: [[BS.ByteString]]
                   -> [BinaryTuple]
binaryDataToTuples = foldl' binaryDataToTuples' []

binaryDataToTuples' :: [BinaryTuple]
                    -> [BS.ByteString]
                    -> [BinaryTuple]
binaryDataToTuples' bt xs = (g, gn):bt
  where
    g:gnxs = xs
    gn = BS.concat gnxs


--convert SNPs to binary values
--For each line, there is the possibility of A vs. all, T vs. all, C vs. all,
-- and G vs. all
convertSnpToBinary ::[[BS.ByteString]]
                   -> [BinaryTuple]
convertSnpToBinary = foldl' convertSnpLineToBinary []


convertSnpLineToBinary ::[BinaryTuple]
                       -> [BS.ByteString]
                       -> [BinaryTuple]
convertSnpLineToBinary bt xs = a:t:c:g:bt
  where
    (geneName:geneData) = xs
    lineWithoutGeneName = BS.concat geneData
    a = (BS.concat [geneName, "_a"], BS.map (replaceChar 'A') lineWithoutGeneName)
    t = (BS.concat [geneName, "_t"], BS.map (replaceChar 'T') lineWithoutGeneName)
    c = (BS.concat [geneName, "_c"], BS.map (replaceChar 'C') lineWithoutGeneName)
    g = (BS.concat [geneName, "_g"], BS.map (replaceChar 'G') lineWithoutGeneName)


replaceChar :: Char
            -> Char
            -> Char
replaceChar matchChar currentChar
    | n == matchChar = '1'
    | n == 'A' = '0'
    | n == 'T' = '0'
    | n == 'C' = '0'
    | n == 'G' = '0'
    | otherwise = n
  where
    n = toUpper currentChar



--this assumes we have stripped the header line and have [(geneName, values)]
getGeneVectorMap :: [BinaryTuple]
                 -> M.HashMap GeneName (V.Vector Char)
getGeneVectorMap = foldl' addGeneData M.empty


--the vData that remains has the delimiters that need to be eliminated
--and the ByteString needs to become a Vector
addGeneData :: M.HashMap GeneName (V.Vector Char)
             -> BinaryTuple
            -> M.HashMap GeneName (V.Vector Char)
addGeneData hm bt = M.insert geneName vectorData hm
  where
    (gName,vData) = bt
    geneName = GeneName gName
    vectorData = V.fromList . BS.unpack  $ vData


--This is the general function for returning a HashMap that contains only those
--entries that match a MetaCategory and [MetaValue]
filterTable :: Table
            -> MetaCategory
            -> FilterType
            -> [MetaValue]
            -> Table
filterTable t mc ft mv = case ft of
                        FilterCategory -> M.filter filterFunc t
                        AllButCategory -> M.filter filterFunc' t
  where
    filterFunc :: MetaHash
               -> Bool
    filterFunc mh = case M.lookup mc mh of
        Just v  -> or $ fmap (== v) mv
        Nothing -> False
    filterFunc' :: MetaHash
                -> Bool
    filterFunc' mh' = case M.lookup mc mh' of
        Just v' -> and $ fmap (/= v') mv
        Nothing -> False

-- |We want to return a datastructure of the groups and categories
-- properly parsed with corresponding Types
generateCategories :: String
                   -> String
                   -> GroupCategories
generateCategories onexs twoxs = MkGroupCategories goc gov gtc gtv
  where
    onexsxs = words onexs
    twoxsxs = words twoxs
    goc = MetaCategory $ BS.pack $ headNote "No group one category given" onexsxs
    gtc = MetaCategory $ BS.pack $ headNote "No group two category given" twoxsxs
    gov = (MetaValue . BS.pack) <$> tailNote "No group one value" onexsxs
    gtv = (MetaValue . BS.pack) <$> tailNote "No group two value" twoxsxs


-- |we only need the headers of the data table to map the names to columns
-- the first column header is blank in the data table or not needed, as it
-- is assumed the first column is gene data

parseDataFile :: Char
              -> UserMode
              -> BS.ByteString
              -> ParsedDataFile
parseDataFile d um bs = MkParsedDataFile ncm gvm
  where
    ncm = assignColumnNumbersToGenome (zip splitGenomeNames [0..])
    dataLines = BS.filter ('\r' /=) <$> BS.lines bs
    genomeNames = headNote "No names present in data file" dataLines
    genomeData = BS.split d <$> tailNote "No data present in data file" dataLines
    splitGenomeNames = BS.split d genomeNames
    finalDataTuples = convertDataToTuples um genomeData
    gvm = getGeneVectorMap finalDataTuples
    
