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


newtype GenomeName = GenomeName {unGenomeName :: BS.ByteString } deriving (Show, Eq, Ord, Generic)
-- newtype Serotype = Serotype { unSerotype :: BS.ByteString } deriving (Show, Eq, Ord, Generic)
-- newtype Source = Source { unSource :: BS.ByteString } deriving (Show, Eq, Ord, Generic)
-- newtype Country = Country { unCountry :: BS.ByteString } deriving (Show, Eq, Ord, Generic)
-- newtype Province = Province { unProvince :: BS.ByteString } deriving (Show, Eq, Ord, Generic)
-- newtype IsolationDate = IsolationDate { unIsolationDate :: Maybe C.Day } deriving (Show, Eq, Ord, Generic)
--newtype ColumnNumber = ColumnNumber { unColumnNumber :: Maybe Int} deriving (Show, Eq, Ord)
newtype GeneName = GeneName { unGeneName :: BS.ByteString } deriving (Eq, Show, Ord, Generic)

--make classes instances of Hashable
--this requires the import of GHC.Generics (Generic) and Data.Hashable
--also requires the Type to derive Generic
--then a new instance for Hashable can be created
instance Hashable GeneName
instance Hashable GenomeName
instance Hashable Metadata



data Metadata = MetaOne {unMetaOne :: BS.ByteString }
              | MetaTwo { unMetaTwo :: BS.ByteString }
              | MetaThree { unMetaThree :: BS.ByteString }
              | MetaFour { unMetaFour :: BS.ByteString }
              | ColumnNumber {unColumnNumber :: Maybe Int}
              deriving (Eq, Show, Ord, Generic)


data Table = Table{metaone      :: Metadata
                  ,metatwo      :: Metadata
                  ,metathree    :: Metadata
                  ,metafour     :: Metadata
                  ,columnNumber :: Metadata } deriving (Eq, Show, Ord, Generic)


fromMetadata :: Metadata -> BS.ByteString
fromMetadata x = case x of
  MetaOne _-> unMetaOne x
  MetaTwo _-> unMetaTwo x
  MetaThree _-> unMetaThree x
  MetaFour _-> unMetaFour x
  ColumnNumber _ -> error("Column number not expected")

defaultTable :: Table
defaultTable = Table
    {metaone = MetaOne defaultNa
    ,metatwo = MetaTwo defaultNa
    ,metathree = MetaThree defaultNa
    ,metafour = MetaFour defaultNa
    ,columnNumber = ColumnNumber Nothing}


getMetadataType :: Metadata -> Table -> Metadata
getMetadataType x = case x of
  MetaOne _ -> metaone
  MetaTwo _ -> metatwo
  MetaThree _ -> metathree
  MetaFour _ -> metafour
  ColumnNumber _ -> columnNumber


defaultNa :: BS.ByteString
defaultNa = BS.pack "NA"



getListOfMetadata :: [(Int,String)] -> [Metadata]
getListOfMetadata = foldl' insertMetadata []
  where
    insertMetadata :: [Metadata]
                   -> (Int, String)
                   -> [Metadata]
    insertMetadata xs (i,s) = let mList = fmap BS.pack . words $ s in
      case i of
        1 -> fmap MetaOne mList ++ xs
        2 -> fmap MetaTwo mList ++ xs
        3 -> fmap MetaThree mList ++ xs
        4 -> fmap MetaFour mList ++ xs
        _ -> error "Unknown metadata type"



--For each line in the file, except the first one, we want to create a record
--with all of the information given in Table
--the first one contains column headers
getMetadataFromFile :: [[BS.ByteString]] -> M.HashMap GenomeName Table
getMetadataFromFile [] = error "File is empty"
getMetadataFromFile (_:xs) = foldl' getEntry M.empty xs


getEntry :: M.HashMap GenomeName Table
         -> [BS.ByteString]
         -> M.HashMap GenomeName Table
getEntry hm [] = hm
getEntry hm (name:xs) = M.insert (GenomeName name) newMeta hm
  where
    newMeta = foldl' getTableValues defaultTable $ zip xs [1..]


--date comes from CSV where in format 2000/01/07
getTableValues :: Table -> (BS.ByteString, Int) -> Table
getTableValues t (x,i)
    | i == 1 = t{metaone = MetaOne x}
    | i == 2 = t{metatwo = MetaTwo x}
    | i == 3 = t{metathree = MetaThree x}
    | i == 4 = t{metafour = MetaFour x}
    | otherwise = t



--we don't want to have to pass default values to the
--public facing functions. Partially apply this
--function to the "workhorse" private function
--that automatically supplies the defaults
--eg. first column contains row names, so skip
addColumnNumbers :: M.HashMap GenomeName Table
                 -> [BS.ByteString]
                 -> M.HashMap GenomeName Table
addColumnNumbers _ [] = error "No data sent to addColumnNumbers"
addColumnNumbers hm (_:xs) = ggn $ zip xs [0..]
  where
    ggn :: [(BS.ByteString, Int)] -> M.HashMap GenomeName Table
    ggn = foldl' addColumnToTable hm
    --we will use the adjust function
    --only changes a value if it exists
    --otherwise, returns the original HashMap
    addColumnToTable :: M.HashMap GenomeName Table
                     -> (BS.ByteString, Int)
                     -> M.HashMap GenomeName Table
    addColumnToTable hm' (x,i) = M.adjust addColumn (GenomeName x) hm'
      where
        addColumn :: Table -> Table
        addColumn t = t{columnNumber = ColumnNumber $ Just i}



getDateFromCSV :: BS.ByteString -> Maybe C.Day
getDateFromCSV x =  case BS.split '/' x of
    [year,month,day] -> Just $ C.fromGregorian (read (BS.unpack year)::Integer)
                          (intValue month) (intValue day)
    _ -> Nothing


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
convertSnpToBinary d = foldl' (convertSnpLineToBinary d) []
  where
    convertSnpLineToBinary :: Char
                           -> [BinaryLine]
                           -> SnpLine
                           -> [BinaryLine]
    convertSnpLineToBinary d' xs sl = a:t:c:g:xs
      where
        (geneName,lineWithoutGeneName) = BS.break (== ',') sl
        replaceChar :: Char
                    -> Char
                    -> Char
        replaceChar matchChar currentChar
            | n == matchChar = '1'
            | n == d' = d'
            | otherwise = '0'
          where
            n = toUpper currentChar
        a = BS.concat [geneName, "a", BS.map (replaceChar 'A') lineWithoutGeneName]
        t = BS.concat [geneName, "t", BS.map (replaceChar 'T') lineWithoutGeneName]
        c = BS.concat [geneName, "c", BS.map (replaceChar 'C') lineWithoutGeneName]
        g = BS.concat [geneName, "g", BS.map (replaceChar 'G') lineWithoutGeneName]

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


-- countByMetadata :: (Table -> Metadata) -> M.HashMap GenomeName Table -> M.HashMap Metadata Int
-- countByMetadata meta = foldl' (addMetadataValue meta) M.empty


-- addMetadataValue :: (Table -> a ) -> M.HashMap a Int -> Table -> M.HashMap a Int
-- addMetadataValue meta hm t = M.insertWith (+) (meta t) 1 hm


-- summaryMetadataTable :: M.HashMap a Int -> [String]
-- summaryMetadataTable hm = sort $ M.foldlWithKey' makeTable [] hm
--   where
--     makeTable :: => [String] -> b -> Int -> [String]
--     makeTable xs name v = (BS.unpack(unMeta name) ++ "\t" ++ show v):xs


--This is the general function for returning a HashMap that contains only those
--entries that matched a particular Metadata entry.
--getMetadataType returns the record syntax FUNCTION that extracts the particular
--type of metadata from a table
filterMetadataInfo :: [Metadata] -> M.HashMap GenomeName Table -> M.HashMap GenomeName Table
filterMetadataInfo xs = M.filter filterFunc
  where
    filterFunc :: Table -> Bool
    filterFunc t = or allComparisons
      where
        allComparisons = fmap currentTest xs
        currentTest :: Metadata
                    -> Bool
        currentTest x = getMetadataType x t == x


--this function takes a pre-filtered data structure
--It returns a list of all column positions for the genomes
getColumnList :: M.HashMap GenomeName Table
              -> [Int]
getColumnList = M.foldl' (\ a v -> (fromJust . unColumnNumber. columnNumber $ v):a ) []


--check the character at each index that is passed in
--returns total count of matches
countCharInVectorByIndices :: V.Vector Char
                           -> Char
                           -> [Int]
                           -> Int
countCharInVectorByIndices v matchChar = foldl' aFun 0
  where
    aFun :: Int -> Int -> Int
    aFun rt vi = if matchChar == (v V.! vi)
          then rt + 1
          else rt




