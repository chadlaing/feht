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
import           Data.Ord
import qualified Data.Set                   as Set
import qualified Data.Vector.Unboxed        as V
import           FET
import           GHC.Generics               (Generic)
import           Prelude                    (Double, abs, fromIntegral, (*),
                                             (+), (-), (/), (>))
import           Table
import           Text.Show
import           UserInput


data Comparison = Comparison{compGroup1 :: Table
                            ,compGroup2 :: Table
                            }deriving (Show, Eq, Generic)
instance Hashable Comparison


data ComparisonResult = MkComparisonResult
  {compFET   :: FETResult
  ,compRatio :: Double
  } deriving(Show, Eq)

type ComparisonResultMap = M.HashMap Comparison [ComparisonResult]



calculateFETFromGene :: GeneName
                     -> V.Vector Char
                     -> Comparison
                     -> FETResult
calculateFETFromGene ( GeneName gn ) vc c = fet (FETName gn) (GroupOneA goa)
  (GroupOneB gob) (GroupTwoA gta) (GroupTwoB gtb) TwoTail
  where
    goa = countCharInVectorByIndices vc '1' goColumnList
    gta = countCharInVectorByIndices vc '1' gtColumnList
    gob = countCharInVectorByIndices vc '0' goColumnList
    gtb = countCharInVectorByIndices vc '0' gtColumnList
    goColumnList = getListOfColumns $ compGroup1 c
    gtColumnList = getListOfColumns $ compGroup2 c


getListOfColumns :: Table
                 -> [Int]
getListOfColumns t = foldl' getColumn [] (M.keys t)
  where
    getColumn :: [Int]
              -> GenomeInfo
              -> [Int]
    getColumn xs gi = columnNumber gi - 1:xs


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



-- |Entry function to sort the results into a table format for printing.
formatComparisonResultsAsTable :: ComparisonResultMap
                               -> [BS.ByteString]
formatComparisonResultsAsTable = M.foldlWithKey' formatComparisonResult []


-- |For each Comparison [ComparisonResult] add the sorted list of markers
-- to the output.
formatComparisonResult :: [BS.ByteString]
                       -> Comparison
                       -> [ComparisonResult]
                       -> [BS.ByteString]
formatComparisonResult xs c cr = x:xs
  where
    x = BS.concat[comparisonHeader, BS.concat comparisonValues]
    comparisonHeader =
      BS.intercalate (BS.singleton '\n') [BS.append "\nGroupOne:" goDescription
                                         ,BS.append "GroupTwo:" gtDescription
                                         ,columnHeader]
    columnHeader ="Name\tGroupOne (+)\tGroupOne (-)\tGroupTwo (+)\tGroupTwo (-)\tpValue\tRatio\n"
    goDescription = lineFromMetaMatch .  createCategoryValueSet $ compGroup1 c
    gtDescription = lineFromMetaMatch . createCategoryValueSet $ compGroup2 c
    comparisonValues = foldl' formatSingleResult [] (sortComparisonResultByRatio cr)


-- |Custom sorting function for [ComparisonResult] by abs(ratio).
-- Correlates with p-value, but useful for sifting large datasets for all / nothing
-- significant matches.
-- The sortBy function generalizes sort, but requires the input to comparing defined.
sortComparisonResultByRatio :: [ComparisonResult]
                            -> [ComparisonResult]
sortComparisonResultByRatio = sortBy (comparing $ abs . compRatio)


-- |The actual formatting of the result for output. A tab-delimited string for each marker.
formatSingleResult :: [BS.ByteString]
                   -> ComparisonResult
                   -> [BS.ByteString]
formatSingleResult xs r = x:xs
  where
    x = BS.concat[rl, "\n"]
    rl = BS.intercalate (BS.singleton '\t') [cName
                                            ,goa
                                            ,gob
                                            ,gta
                                            ,gtb
                                            ,pv
                                            ,ratio]
    cName = fetName . compFET $ r
    goa = getRatioFromFET groupOneA r
    gob = getRatioFromFET groupOneB r
    gta = getRatioFromFET groupTwoA r
    gtb = getRatioFromFET groupTwoB r
    pv = BS.pack . show . pvalue . compFET $ r
    ratio = BS.pack . show . compRatio $ r


-- |Convert a ratio value within the result to a ByteString
-- for building up the result lines for printing
getRatioFromFET :: (FETResult -> Int)
                -> ComparisonResult
                -> BS.ByteString
getRatioFromFET f = BS.pack . show . f . compFET


-- |Provides the groups and the group members for the comparison
-- as a ByteString for building up the result lines for printing
lineFromMetaMatch :: MetaCategorySet
                  -> BS.ByteString
lineFromMetaMatch = M.foldlWithKey' makeLine ""
  where
    makeLine :: BS.ByteString
             -> MetaCategory
             -> Set.Set MetaValue
             -> BS.ByteString
    makeLine bs (MetaCategory mc) xs = BS.intercalate " " [bs, newLine]
      where
        setAsList = Set.toList xs
        newLine = BS.concat [mc, ":", mValues]
        mValues = BS.intercalate ","  (fmap unMetaValue setAsList)

-- |The Categories and all values for each category are stored in a HashMap
-- that is from the category to a set of all values
type MetaCategorySet = M.HashMap MetaCategory (Set.Set MetaValue)

createCategoryValueSet :: Table
                       -> MetaCategorySet
createCategoryValueSet t = createSetsFromValues (M.elems t)


-- |Builds up the MetaCategorySet from the values stored in the
-- original Table
createSetsFromValues :: [M.HashMap MetaCategory MetaValue]
                     -> MetaCategorySet
createSetsFromValues = foldl' addToSet M.empty


-- | Each M.HashMap MetaCategory MetaValue should be added to the
-- MetaCategorySet
addToSet :: MetaCategorySet
         -> M.HashMap MetaCategory MetaValue
         -> MetaCategorySet
addToSet = M.foldlWithKey' insertInSet


-- |The value is a set of all MetaValues for the given MetaCategory.
-- We can't just insert a new entry into the Set, we have to use insertWith,
-- which gives us access to the current value in the HashMap, which is the
-- Set of MetaValues, which we can then add the new MetaValue to.
-- insertWith takes newValue, oldValue, and returns the value based on
-- the results of the function. We have no need to insert a new Set, so
-- we use the Set.empty as a dummy value, and modify the old Set.
insertInSet :: MetaCategorySet
            -> MetaCategory
            -> MetaValue
            -> MetaCategorySet
insertInSet mcs mc mv = M.insertWith insertNextValue mc Set.empty mcs
  where
    insertNextValue _ = Set.insert mv


-- |Entry point for multiple testing correction of all the comparisons.
-- One of the defined corrections, or no correction at all is applied.
applyMultipleTestingCorrection :: Correction
                               -> ComparisonResultMap
                               -> ComparisonResultMap
applyMultipleTestingCorrection Bonferroni crm = M.map bonferroniCorrectResultMap crm
applyMultipleTestingCorrection _ crm = crm


-- |Entry point for the Bonferroni correction of the pvalues.
-- In a controversial move, the pvalues are themselves adjusted to reflect the
-- new alpha value. Eg. alpha = p / n would be the new cutoff of significance.
-- Instead we provide (p * n) <= 1.0 for each comparison.
bonferroniCorrectResultMap :: [ComparisonResult]
                           -> [ComparisonResult]
bonferroniCorrectResultMap crxs = fmap (bonferroniCorrectComparisonResult (length crxs)) crxs


-- |Corrects the pvalue for individual results.
bonferroniCorrectComparisonResult :: Int
                                  -> ComparisonResult
                                  -> ComparisonResult
bonferroniCorrectComparisonResult nComps (MkComparisonResult cfet crat)
  = MkComparisonResult cfet{pvalue = adjustedValue} crat
     where
       newP = pvalue cfet * fromIntegral nComps
       adjustedValue = if newP > 1
                         then 1
                         else newP


--   |Entry point for creating lists of all the comparisons that need to be
--     computed.  Allows for one vs. all, as well as comparisons between
--     specifically designated groups.

getComparisonList :: Table
                  -> GroupCategories
                  -> [Comparison]
getComparisonList t (MkGroupCategories mc1 mxs1 mc2 mxs2) =
  [Comparison {compGroup1 = cg1, compGroup2 = cg2}]
  where
      cg1 = filterTable t mc1 FilterCategory mxs1
      cg2 = filterTable t mc2 FilterCategory mxs2

  -- | unMetaCategory mc1 == "allbut" = M.foldl' getAllPermutations [] (createCategoryValueSet t)
  -- | unMetaCategory mc2 == "allbut" = [Comparison {compGroup1 = cg1, compGroup2 = cg2'}]
  -- | otherwise = [Comparison {compGroup1 = cg1, compGroup2 = cg2}]
  --   where
  --     cg1 = filterTable t mc1 FilterCategory mxs1
  --     cg2 = filterTable t mc2 FilterCategory mxs2
  --     cg2' = filterTable t mc1 AllButCategory mxs1
  --     getAllPermutations :: [Comparison]
  --                        -> MetaCategorySet
  --                        -> [Comparison]
  --     --getAllPermutations cs (mc', mxs') = foldl' getCatPerms cs mxs'
  --     getAllPermutations cs mcs = M.foldl' getCatPerms [] mcs
  --       where
  --         getCatPerms :: [Comparison]
  --                     -> Set.Set MetaValue
  --                     -> [Comparison]
  --         getCatPerms cs' mvs= cvs:permList cs' (takeWhile (/= mv) mxs')
  --           where
  --             mv = Set.toList mvs
  --             cvs = Comparison {compGroup1 = cg1', compGroup2 = cvs2}
  --             cvs2 = filterTable t mc' AllButCategory [mv]
  --             cg1' = filterTable t mc' FilterCategory [mv]
  --             permList :: [Comparison]
  --                      -> [MetaValue]
  --                      -> [Comparison]
  --             permList cs'' [] = cs''
  --             permList cs'' (x:xs) = permList (c:cs'') xs
  --               where c = Comparison {compGroup1 = cg1, compGroup2 = cg2''}
  --                    cg2'' = filterTable t mc' FilterCategory [x]
    -- c is the comparison given the two values cvs is the
    -- comparison vs the value and all others


