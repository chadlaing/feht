{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

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
import           Data.List                  (zip)
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics               (Generic)
import           Prelude                    (String, error, words)
import           Text.Read
import           Text.Show
import           UserInput
import Safe


newtype GeneName = GeneName { unGeneName :: BS.ByteString } deriving (Eq, Show, Ord, Generic)
newtype MetaCategory = MetaCategory {unMetaCategory :: BS.ByteString} deriving (Eq, Show, Ord, Generic)
newtype MetaValue = MetaValue {unMetaValue :: BS.ByteString} deriving (Eq, Show, Ord, Generic)

data GenomeInfo =
    GenomeInfo{genomeName   :: BS.ByteString
              ,columnNumber :: Int} deriving (Show, Eq, Ord, Generic)


data FilterType = FilterCategory | AllButCategory deriving (Ord, Read, Show, Eq)

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

--For each line in the file, except the first one, we want to create a record
--with all of the information given in Table
--the first one contains column headers
--The first line will be used to get the MetaCategory items
getMetadataFromFile :: GenomeNameColumnHash
                    -> [[BS.ByteString]]
                    -> Table
getMetadataFromFile _ [] = error "File is empty"
getMetadataFromFile _ ([]:_) = error "File is empty"
getMetadataFromFile gnch ((_:header):xs) = foldl' getEntry M.empty xs
  where
    getEntry :: Table
             -> [BS.ByteString]
             -> Table
    getEntry _ []  = error "Cannot get entry from file"
    getEntry t (gName:xs') = M.insert GenomeInfo {genomeName = gName, columnNumber = cNum} metaHash t
      where
        cNum = fromMaybe (error (BS.unpack gName)) $ M.lookup gName gnch
        metaHash = foldl' metaAssign M.empty alignedMeta
          where
            metaAssign :: MetaHash
                       -> (BS.ByteString, BS.ByteString)
                       -> MetaHash
            metaAssign t' (category, value) = M.insert (MetaCategory category) (MetaValue value) t'
            -- this will create a list of ByteString tuples, that need to be made
            -- into the appropriate MetaCategory and values
            alignedMeta = zip header xs'


type GenomeNameColumnHash = M.HashMap BS.ByteString Int
assignColumnNumbersToGenome :: [(BS.ByteString, Int)]
                            -> GenomeNameColumnHash
assignColumnNumbersToGenome = foldl' assignColumn M.empty
  where
    assignColumn :: GenomeNameColumnHash
                 -> (BS.ByteString, Int)
                 -> GenomeNameColumnHash
    assignColumn g (n, i) = M.insert n i g

intValue :: BS.ByteString -> Int
intValue x = read (BS.unpack x)::Int

type BinaryTuple = (BS.ByteString, BS.ByteString)
type SnpLine = BS.ByteString
type BinaryLine = BS.ByteString


convertDataToTuples :: UserMode
                    -> Char
                    -> [BS.ByteString]
                    -> [BinaryTuple]
convertDataToTuples m d xs
  | m == Snp = convertSnpToBinary d xs
  | otherwise = binaryDataToTuples d xs


-- |We are looking to create tuples of the geneID / values
-- split on the delimiter
binaryDataToTuples :: Char
                   -> [BinaryLine]
                   -> [BinaryTuple]
binaryDataToTuples d = foldl' (binaryDataToTuples' d) []

binaryDataToTuples' :: Char
                    -> [BinaryTuple]
                    -> BinaryLine
                    -> [BinaryTuple]
binaryDataToTuples' d' xs bl = (g, gn):xs
  where
    (g, gn) = getTupleFromLine d' bl


--convert SNPs to binary values
--For each line, there is the possibility of A vs. all, T vs. all, C vs. all,
-- and G vs. all
convertSnpToBinary :: Char
                   -> [SnpLine]
                   -> [BinaryTuple]
convertSnpToBinary d = foldl' (convertSnpLineToBinary d) []


convertSnpLineToBinary :: Char
                       -> [BinaryTuple]
                       -> SnpLine
                       -> [BinaryTuple]
convertSnpLineToBinary d' xs sl = a:t:c:g:xs
  where
    (geneName, lineWithoutGeneName) = getTupleFromLine d' sl
    a = (BS.concat [geneName, "_a"], BS.map (replaceChar 'A') lineWithoutGeneName)
    t = (BS.concat [geneName, "_t"], BS.map (replaceChar 'T') lineWithoutGeneName)
    c = (BS.concat [geneName, "_c"], BS.map (replaceChar 'C') lineWithoutGeneName)
    g = (BS.concat [geneName, "_g"], BS.map (replaceChar 'G') lineWithoutGeneName)


getTupleFromLine :: Char
                 -> BS.ByteString
                 -> (BS.ByteString, BS.ByteString)
getTupleFromLine d bs = (gid, sanitizedValues)
  where
    (gid:gvalues) = BS.split d bs
    sanitizedValues = BS.concat . BS.words . BS.unwords $ gvalues

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


-- |Store the categories and associated values for the analyses
data GroupCategories =
  MkGroupCategories{
  oneCategory  :: MetaCategory
  ,oneValues   :: [MetaValue]
  ,twoCategory :: MetaCategory
  ,twoValues   :: [MetaValue]}

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
    gov = (MetaValue . BS.pack) <$> tailSafe onexsxs
    gtv = (MetaValue . BS.pack) <$> tailSafe twoxsxs
