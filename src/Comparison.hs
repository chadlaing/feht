{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Comparison where
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
import           FET
import           GHC.Generics               (Generic)
import           Prelude                    (Double, error, fromIntegral,
                                             length, otherwise, undefined, (+),
                                             (++), (-), (/))
import           Safe                       (tailDef)
import           Table
import           Text.Show
--
--
data Comparison = Comparison{compGroup1 :: Table
                            ,compGroup2 :: Table
                            }deriving (Show, Eq, Generic)
instance Hashable Comparison


data ComparisonResult = MkComparisonResult
  {compFET   :: FETResult
  ,compRatio :: Double
  } deriving(Show, Eq)

-- generate fet
-- generate compRatio
-- create ComparisonResult
-- add to hash

type GeneVectorMap = M.HashMap GeneName (V.Vector Char)
type ComparisonResultMap = M.HashMap Comparison [ComparisonResult]

calculateFETFromGene :: GeneName
                     -> V.Vector Char
                     -> Comparison
                     -> FETResult
calculateFETFromGene (GeneName gn) vc c = fet (FETName gn) (GroupOneA goa) (GroupOneB gob) (GroupTwoA gta) (GroupTwoB gtb) TwoTail
  where
    goa = countCharInVectorByIndices vc '1' goColumnList
    gta = countCharInVectorByIndices vc '1' gtColumnList
    gob = countCharInVectorByIndices vc '0' goColumnList
    gtb = countCharInVectorByIndices vc '0' gtColumnList
    goColumnList = getListOfColumns $ compGroup1 c
    gtColumnList = getListOfColumns $ compGroup2 c


calculateRatio :: FETResult
               -> Double
calculateRatio fetr = (goa / (goa + gob)) - (gta / (gta + gtb))
  where
    goa = fromIntegral $ groupOneA fetr
    gob = fromIntegral $ groupOneB fetr
    gta = fromIntegral $ groupTwoA fetr
    gtb = fromIntegral $ groupTwoB fetr


generateResultMap :: GeneVectorMap
                    -> [Comparison]
                    -> ComparisonResultMap
generateResultMap gvm = foldl' (generateComparisonResults gvm) M.empty


generateComparisonResults :: GeneVectorMap
                          -> ComparisonResultMap
                          -> Comparison
                          -> ComparisonResultMap
generateComparisonResults gvm crm c = M.insert c xsComparison crm
  where
    xsComparison = M.foldlWithKey' (generateComparisonResult c) [] gvm 


generateComparisonResult :: Comparison
                   -> [ComparisonResult]
                   -> GeneName
                   -> V.Vector Char
                   -> [ComparisonResult]
generateComparisonResult c xs gn vc = x:xs
  where
    x = MkComparisonResult cFET cRatio
    cFET = calculateFETFromGene gn vc c
    cRatio = calculateRatio cFET




--calculateFetFromComparison :: GeneVectorHash
--                           -> Comparison
--                           -> FETResultHash
--calculateFetFromComparison gvh c = M.insert c allFetResults M.empty
--  where
--    allFetResults :: [FETResult]
--    allFetResults = M.foldlWithKey' calcFet [] gvh
--      where
--        calcFet :: [FETResult]
--                -> GeneName
--                -> V.Vector Char
--                -> [FETResult]
--        calcFet xs gn vc = theResult:xs
--          where
--            goColumnList = getListOfColumns $ compGroup1 c
--            gtColumnList = getListOfColumns $ compGroup2 c
--            got = M.size $ compGroup1 c
--            gtt = M.size $ compGroup2 c
--            goa = countCharInVectorByIndices vc '1' goColumnList
--            gta = countCharInVectorByIndices vc '1' gtColumnList
--            gob = countCharInVectorByIndices vc '0' goColumnList
--            gtb = countCharInVectorByIndices vc '0' gtColumnList
----            gob = got - goa
----            gtb = gtt - gta
--            theResult = fet (FETName $ unGeneName gn) (GroupOneA goa) (GroupOneB gob) (GroupTwoA gta)
--                (GroupTwoB gtb) TwoTail
--


getListOfColumns :: Table
                 -> [Int]
getListOfColumns t = foldl' getColumn [] (M.keys t)
  where
    getColumn :: [Int]
              -> GenomeInfo
              -> [Int]
    getColumn xs gi = columnNumber gi - 1:xs



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


--
--formatFETResultHashAsTable :: FETResultHash
--                           -> [BS.ByteString]
--formatFETResultHashAsTable = M.foldlWithKey' formatFETResult []
--  where
--    formatFETResult :: [BS.ByteString]
--                    -> Comparison
--                    -> [FETResult]
--                    -> [BS.ByteString]
--    formatFETResult xs c fr = newComparison ++ xs
--      where
--       newComparison = newHeader:allResults
--       groupOneDescription = lineFromMetaMatch . getAllMetaValue $ compGroup1 c
--       groupTwoDescription = lineFromMetaMatch . getAllMetaValue $ compGroup2 c
--       groupHeader = "Name\tGroupOne (+)\tGroupOne (-)\tGroupTwo (+)\tGroupTwo (-)\tpValue"
--       newHeader = BS.intercalate (BS.singleton '\n') [BS.append "\nGroupOne:" groupOneDescription, BS.append "GroupTwo:" groupTwoDescription, groupHeader]
--       allResults = foldl' formatSingleFET [] fr
--         where
--           formatSingleFET :: [BS.ByteString]
--                           -> FETResult
--                           -> [BS.ByteString]
--           formatSingleFET xs fr' = newFet:xs
--             where
--                newFet = BS.intercalate (BS.singleton '\t')
--                          [fetName fr'
--                          ,BS.pack . show . groupOneA $ fr'
--                          ,BS.pack . show . groupOneB $ fr'
--                          ,BS.pack . show . groupTwoA $ fr'
--                          ,BS.pack . show . groupTwoB $ fr'
--                          ,BS.pack . show . pvalue $ fr'
--                          ]



lineFromMetaMatch :: [MetaMatch]
                  -> BS.ByteString
lineFromMetaMatch = foldl' makeLine ""
  where
    makeLine :: BS.ByteString
             -> MetaMatch
             -> BS.ByteString
    makeLine bs (MetaCategory mc, xs) = BS.intercalate " " [bs, newLine]
      where
        newLine = BS.concat [mc, ":", mValues]
        mValues = BS.intercalate ","  (fmap unMetaValue xs)


--Table is Hash of Hash
--Get the values of the HoH, get unique elements, then combine into single
--ByteString
type MetaMatch = (MetaCategory, [MetaValue])
getAllMetaValue :: Table
                -> [MetaMatch]
getAllMetaValue t = foldl' getValueList [] allCategories
  where
    allMetaHash = M.elems t
    allCategories = getAllCategoriesFromTable t
    getValueList :: [MetaMatch]
                 -> MetaCategory
                 -> [MetaMatch]
    getValueList xs m = x:xs
      where
        x = (m , nub $ foldl' gvl [] allMetaHash)
        gvl :: [MetaValue]
            -> MetaHash
            -> [MetaValue]
        gvl xs mh = fromMaybe (error "MetaCategory does not exist") (M.lookup m mh):xs


-- filterComparisonsByPValue :: FETResultHash
--                           -> FETResultHash
-- filterComparisonsByPValue = M.foldlWithKey' isSignificant M.empty
--   where
--     isSignificant :: FETResultHash
--                   -> Comparison
--                   -> [FETResult]
--                   -> FETResultHash
--     isSignificant hm k fr = if null filteredList
--                                 then hm
--                                 else M.insert k filteredList hm
--       where
--         filteredList = filter (\x -> pvalue x < correctedCutoff) fr
--         correctedCutoff = 0.05 / fromIntegral numberOfComparisons
--         numberOfComparisons = length fr


getComparisonList :: Table
                  -> MetaMatch
                  -> MetaMatch
                  -> [Comparison]
getComparisonList t (mc1, mxs1) (mc2, mxs2)
    | unMetaCategory mc1 == "allbut" = foldl' getAllPermutations [] (getAllMetaValue t)
    | unMetaCategory mc2 == "allbut" = [Comparison {compGroup1 = cg1, compGroup2 = cg2'}]
    | otherwise = [Comparison {compGroup1 = cg1, compGroup2 = cg2}]
  where
    mm1 = (mc1, mxs1)
    mm2 = (mc2, mxs2)
    cg1 = filterTable t mc1 FilterCategory mxs1
    cg2 = filterTable t mc2 FilterCategory mxs2
    cg2' = filterTable t mc1 AllButCategory mxs1
    getAllPermutations :: [Comparison]
                       -> MetaMatch
                       -> [Comparison]
    getAllPermutations cs (mc', mxs') = foldl' getCatPerms cs mxs'
      where
        getCatPerms :: [Comparison]
                    -> MetaValue
                    -> [Comparison]
        getCatPerms cs' mv = cvs:permList cs' (takeWhile (/= mv) mxs')
          where
            cvs = Comparison {compGroup1 = cg1, compGroup2 = cvs2}
            cvs2 = filterTable t mc' AllButCategory [mv]
            cg1 = filterTable t mc' FilterCategory [mv]
            permList :: [Comparison]
                     -> [MetaValue]
                     -> [Comparison]
            permList cs'' [] = cs''
            permList cs'' (x:xs) = permList (c:cs'') xs
              where
                c = Comparison {compGroup1 = cg1, compGroup2 = cg2''}
                cg2'' = filterTable t mc' FilterCategory [x]
            -- c is the comparison given the two values
            -- cvs is the comparison vs the value and all others


getAllCategoriesFromTable :: Table
                          -> [MetaCategory]
getAllCategoriesFromTable t = M.keys $ fromMaybe (error "Genome does not exist") (M.lookup singleGenome t)
  where
    singleGenome = head . M.keys $ t
