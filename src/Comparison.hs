{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings          #-}

module Comparison where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Eq
import           Data.Int
import           Data.Foldable
import           Data.Functor
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import           Data.List
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics               (Generic)
import           FET
import           Table
import           Prelude                    (fromIntegral, length, (++),(+), (-),
                                             (/), error)
import           Text.Show
--
--
data Comparison = Comparison{compGroup1  :: Table
                            ,compGroup2  :: Table
                            }deriving (Show, Eq, Generic)
instance Hashable Comparison




type GeneVectorHash = M.HashMap GeneName (V.Vector Char)
type FETResultHash = M.HashMap Comparison [FETResult]
calculateFetFromComparison :: Comparison
                           -> GeneVectorHash
                           -> FETResultHash
calculateFetFromComparison c gvh = M.insert c allFetResults M.empty
  where
    allFetResults :: [FETResult]
    allFetResults = M.foldlWithKey' calcFet [] gvh
      where
        calcFet :: [FETResult]
                -> GeneName
                -> V.Vector Char
                -> [FETResult]
        calcFet xs gn vc = theResult:xs
          where
            goColumnList = getListOfColumns $ compGroup1 c
            gtColumnList = getListOfColumns $ compGroup2 c
            got = M.size $ compGroup1 c
            gtt = M.size $ compGroup2 c
            goa = countCharInVectorByIndices vc '1' goColumnList
            gta = countCharInVectorByIndices vc '1' gtColumnList
            gob = got - goa
            gtb = gtt - gta
            theResult = fet (FETName $ unGeneName gn) (GroupOneA goa) (GroupOneB gob) (GroupTwoA gta)
                (GroupTwoB gtb) TwoTail



getListOfColumns :: Table
                 -> [Int]
getListOfColumns t = foldl' getColumn [] (M.keys t)
  where
    getColumn :: [Int]
              -> GenomeInfo
              -> [Int]
    getColumn xs gi = columnNumber gi:xs



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



formatFETResultHashAsTable :: FETResultHash
                           -> [BS.ByteString]
formatFETResultHashAsTable = M.foldlWithKey' formatFETResult []
  where
    formatFETResult :: [BS.ByteString]
                    -> Comparison
                    -> [FETResult]
                    -> [BS.ByteString]
    formatFETResult xs c fr = newComparison ++ xs
      where
       newComparison = newHeader:allResults
       groupOneDescription = getAllMetaValue $ compGroup1 c
       groupTwoDescription = getAllMetaValue $ compGroup2 c
       groupHeader = "Name\tGroupOne (+)\tGroupOne (-)\tGroupTwo (+)\tGroubTwo (-)\tpValue"
       newHeader = BS.intercalate (BS.singleton '\n') [BS.append "GroupOne:" groupOneDescription, BS.append "GroupTwo:" groupTwoDescription, groupHeader]
       allResults = foldl' formatSingleFET [] fr
         where
           formatSingleFET :: [BS.ByteString]
                           -> FETResult
                           -> [BS.ByteString]
           formatSingleFET xs fr' = newFet:xs
             where
                newFet = BS.intercalate (BS.singleton '\t')
                          [fetName fr'
                          ,BS.pack . show . groupOneA $ fr'
                          ,BS.pack . show . groupOneB $ fr'
                          ,BS.pack . show . groupTwoA $ fr'
                          ,BS.pack . show . groupTwoB $ fr'
                          ,BS.pack . show . pvalue $ fr'
                          ]

--Table is Hash of Hash
--Get the values of the HoH, get unique elements, then combine into single
--ByteString
getAllMetaValue :: Table
                -> BS.ByteString
getAllMetaValue t = BS.intercalate (BS.singleton ' ') finalList
  where
    allMetaHash = M.elems t
    allCategories = nub . concatMap M.keys $ allMetaHash
    finalList = foldl' getValueList [] allCategories
      where
        getValueList :: [BS.ByteString]
                     -> MetaCategory
                     -> [BS.ByteString]
        getValueList xs m = x:xs
          where
            x = BS.intercalate (BS.singleton ':') [unMetaCategory m, stringOfValues]
            stringOfValues = BS.intercalate (BS.singleton ',') $ nub allValues
            allValues = foldl' gvl [] allMetaHash
              where
                gvl :: [BS.ByteString]
                    -> MetaHash
                    -> [BS.ByteString]
                gvl xs mh = unMetaValue (fromMaybe (error "MetaCategory does not exist") (M.lookup m mh)):xs





-- . nub . fmap unMetaValue . concatMap M.elems . M.elems


--     formatFETResult xs c fr = newComparison ++ xs
--       where
--         columnHeaders = BS.pack "Name\tGroupOneA(+)\tGroupOneB(-)\tGroupTwoA(+)\tGroubTwoB(-)\tpValue"
--         newComparison = getHeader c:columnHeaders:xs'
--         getHeader :: Comparison -> BS.ByteString
--         getHeader c' = BS.intercalate (BS.singleton '\t')
--                          [fromMetadata $ compGroup1 c'
--                          ,maybe (BS.pack "all") fromMetadata $ compGroup2 c'
--                          ,maybe (BS.pack "no filter") fromMetadata $ filterGroup c'
--                          ]
--         xs' = foldl' getFetLine [] fr
--           where
--             getFetLine :: [BS.ByteString] -> FETResult -> [BS.ByteString]
--             getFetLine xs'' fr' = newResult:xs''
--               where
--                 newResult = BS.intercalate (BS.singleton '\t')
--                               [fetName fr'
--                               ,BS.pack . show . groupOneA $ fr'
--                               ,BS.pack . show . groupOneB $ fr'
--                               ,BS.pack . show . groupTwoA $ fr'
--                               ,BS.pack . show . groupTwoB $ fr'
--                               ,BS.pack . show . pvalue $ fr'
--                               ]
--
--
-- filterComparisonsByPValue :: M.HashMap Comparison [FETResult]
--                           -> M.HashMap Comparison [FETResult]
-- filterComparisonsByPValue = M.foldlWithKey' isSignificant M.empty
--   where
--     isSignificant :: M.HashMap Comparison [FETResult]
--                   -> Comparison
--                   -> [FETResult]
--                   -> M.HashMap Comparison [FETResult]
--     isSignificant hm k fr = M.insert k filteredList hm
--       where
--         filteredList = filter (\x -> pvalue x < correctedCutoff) fr
--         correctedCutoff = 0.05 / fromIntegral numberOfComparisons
--         numberOfComparisons = length fr
--
--
--
