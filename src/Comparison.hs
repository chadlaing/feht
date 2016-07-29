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
import           Prelude                    (fromIntegral, length, (++),(+), (-), undefined, (/), error)
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
       groupOneDescription = BS.intercalate (BS.singleton ',') . fmap unMetaValue . getAllMetaValue $ compGroup1 c
       groupTwoDescription = BS.intercalate (BS.singleton ',') . fmap unMetaValue . getAllMetaValue $ compGroup2 c
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
                -> [MetaValue]
getAllMetaValue t = foldl' getValueList [] allCategories
  where
    allMetaHash = M.elems t
    allCategories = getAllCategoriesFromTable t
    getValueList :: [MetaValue]
                 -> MetaCategory
                 -> [MetaValue]
    getValueList xs m = x ++ xs
      where
        x = nub $ foldl' gvl [] allMetaHash
        gvl :: [MetaValue]
            -> MetaHash
            -> [MetaValue]
        gvl xs mh = fromMaybe (error "MetaCategory does not exist") (M.lookup m mh):xs


filterComparisonsByPValue :: FETResultHash
                          -> FETResultHash
filterComparisonsByPValue = M.foldlWithKey' isSignificant M.empty
  where
    isSignificant :: FETResultHash
                  -> Comparison
                  -> [FETResult]
                  -> FETResultHash
    isSignificant hm k fr = M.insert k filteredList hm
      where
        filteredList = filter (\x -> pvalue x < correctedCutoff) fr
        correctedCutoff = 0.05 / fromIntegral numberOfComparisons
        numberOfComparisons = length fr



generateListOfAllComparisons :: Table
                             -> [Comparison]
generateListOfAllComparisons t = undefined
  where
    allCategories = getAllCategoriesFromTable t



getAllCategoriesFromTable :: Table
                          -> [MetaCategory]
getAllCategoriesFromTable t = M.keys $ fromMaybe (error "Genome does not exist") (M.lookup singleGenome t)
  where
    singleGenome = head . M.keys $ t