{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Table where

import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Eq
import           Data.Foldable
import           Data.Function
import           Data.Functor               (fmap)
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import           Data.Int
import           Data.List                  (zip, (++))
import           Data.Maybe
import           Data.Ord
import           Data.String
import qualified Data.Time.Calendar         as C
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics               (Generic)
import           Prelude                    (Integer, error, (+), undefined)
import           Text.Read
import           Text.Show


newtype GeneName = GeneName { unGeneName :: BS.ByteString } deriving (Eq, Show, Ord, Generic)
newtype MetaCategory = MetaCategory {unMetaCategory :: BS.ByteString} deriving (Eq, Show, Ord, Generic)
newtype MetaValue = MetaValue {unMetaValue :: BS.ByteString} deriving (Eq, Show, Ord, Generic)

data GenomeInfo =
    GenomeInfo{genomeName :: BS.ByteString
              ,columnNumber :: Int} deriving (Show, Eq, Ord, Generic)

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
getMetadataFromFile gnch ((blank:header):xs) = foldl' getEntry M.empty xs
  where
    getEntry :: Table
             -> [BS.ByteString]
             -> Table
    getEntry t (gName:xs') = M.insert GenomeInfo {genomeName = gName, columnNumber = cNum} metaHash t
      where
        cNum = fromMaybe (error "There are genome names in the data that do not exist in the metadata") $ M.lookup gName gnch
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



--convert SNPs to binary values
--For each line, there is the possibility of A vs. all, T vs. all, C vs. all,
-- and G vs. all
type BinaryLine = BS.ByteString
type SnpLine = BS.ByteString
convertSnpToBinary :: Char
                   -> [SnpLine]
                   -> [BinaryLine]
convertSnpToBinary d = foldl' convertSnpLineToBinary []
  where
    convertSnpLineToBinary :: [BinaryLine]
                           -> SnpLine
                           -> [BinaryLine]
    convertSnpLineToBinary xs sl = a:t:c:g:xs
      where
        (geneName,lineWithoutGeneName) = BS.break (== d) sl
        replaceChar :: Char
                    -> Char
                    -> Char
        replaceChar matchChar currentChar
            | n == matchChar = '1'
            | n == d = d
            | otherwise = '0'
          where
            n = toUpper currentChar
        a = BS.concat [geneName, "_a", BS.map (replaceChar 'A') lineWithoutGeneName]
        t = BS.concat [geneName, "_t", BS.map (replaceChar 'T') lineWithoutGeneName]
        c = BS.concat [geneName, "_c", BS.map (replaceChar 'C') lineWithoutGeneName]
        g = BS.concat [geneName, "_g", BS.map (replaceChar 'G') lineWithoutGeneName]

--this assumes we have stripped the header line and have [[genename:values]]
--the char is the delimiter used for splitting each data line
getGeneVectorMap :: Char
                 -> [BS.ByteString]
                 -> M.HashMap GeneName (V.Vector Char)
getGeneVectorMap delimiter = foldl' addGeneData M.empty
  where
    --each line of the input file is delimited
    --the gene name is first, followed by the single-char values for each genome
    --we break each line into gene name and data with break
    --the vData that remains has the delimiters that need to be eliminated
    --and the ByteString needs to become a Vector
    addGeneData :: M.HashMap GeneName (V.Vector Char)
                 -> BS.ByteString
                -> M.HashMap GeneName (V.Vector Char)
    addGeneData hm xs
        | BS.null xs = error "No gene data given"
        | otherwise = M.insert geneName vectorData hm
      where
        geneAndData = BS.break (== delimiter) xs
        (gName,vData) = geneAndData
        geneName = GeneName gName
        vectorData = V.fromList . BS.unpack . BS.filter (/= delimiter) $ vData


--This is the general function for returning a HashMap that contains only those
--entries that match a MetaCategory and [MetaValue]
filterTable :: Table
            -> MetaCategory
            -> [MetaValue]
            -> Table
filterTable t mc mv = M.filter filterFunc t
  where
    filterFunc :: MetaHash
               -> Bool
    filterFunc mh = case M.lookup mc mh of
        Just theValue -> or $ fmap (== theValue) mv
        Nothing -> False


