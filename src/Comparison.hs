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
import           Prelude                    (Double, abs, error, fromIntegral,
                                             otherwise, undefined, (*), (+),
                                             (-), (/), (>))
import           Table
import           Text.Show
import           UserInput


data Comparison = Comparison{compGroup1  :: Table
                            ,compGroup2  :: Table
                            ,compDetails :: BS.ByteString
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
    getColumn xs gi = columnNumber gi:xs


calculateRatio :: FETResult
               -> Double
calculateRatio fetr = (goa / (goa + gob)) - (gta / (gta + gtb))
  where
    goa = fromIntegral $ groupOneA fetr
    gob = fromIntegral $ groupOneB fetr
    gta = fromIntegral $ groupTwoA fetr
    gtb = fromIntegral $ groupTwoB fetr

-- |Given the list of comparisons, create a map containing
-- the results for each comparison
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


-- |For each comparison, get the Fisher's Exact and Ratio value
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
formatComparisonResultsAsTable :: Double
                               -> ComparisonResultMap
                               -> [BS.ByteString]
formatComparisonResultsAsTable d =
  M.foldlWithKey' (formatComparisonResult d) []


-- |For each Comparison [ComparisonResult] add the sorted list of markers
-- to the output. Before sorting them, remove any that are below the
-- ratioFilter.
formatComparisonResult :: Double
                       -> [BS.ByteString]
                       -> Comparison
                       -> [ComparisonResult]
                       -> [BS.ByteString]
formatComparisonResult d xs c cr = x:xs
  where
    cr' = filter (\z -> d < (abs . compRatio $ z)) cr
    x = BS.concat[comparisonHeader, BS.concat comparisonValues]
    comparisonHeader =
      BS.intercalate (BS.singleton '\n') [compDetails c
                                         ,columnHeader]
    columnHeader ="Name\tGroupOne (+)\tGroupOne (-)\tGroupTwo (+)\tGroupTwo (-)\tpValue\tRatio\n"
    -- comparisonValues = foldl' formatSingleResult [] (sortComparisonResultByRatio cr')
    comparisonValues = foldl' formatSingleResult [] cr
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
-- We output the corrected pvalues, which are themselves adjusted to reflect the
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
getComparisonList t (MkGroupCategories mc1 mxs1 mc2 mxs2)
  | unMetaCategory mc1 == "all" = M.foldlWithKey' (getAllPermutations t) [] (createCategoryValueSet t)
  | unMetaCategory mc2 == "all" = comp'
  | otherwise = comp
    where
      comp' = [Comparison cg1 cg2' cd']
      comp = [Comparison cg1 cg2 cd]
      cd = createComparisonDescription mc1 mxs1 mc2 mxs2
      cd' = createComparisonDescription mc1 mxs1 mc1 []
      cg1 = filterTable t mc1 FilterCategory mxs1
      cg2 = filterTable t mc2 FilterCategory mxs2
      cg2' = filterTable t mc1 AllButCategory mxs1

-- |Consistent means of creating a description of the comparison.
-- Requires both MetaCategories (usually, but not required to be the same)
-- If the second [] is empty, assumes a one vs. all comparison.
createComparisonDescription :: MetaCategory
                            -> [MetaValue]
                            -> MetaCategory
                            -> [MetaValue]
                            -> BS.ByteString
createComparisonDescription xc xs yc ys =
  BS.concat["Group1 category: "
           ,unMetaCategory xc
           ,g1d
           ,"Group2 category: "
           ,unMetaCategory yc
           ,g2d]
  where
    g1d = case xs of
      [] -> error "No group 1 values"
      _ -> BS.concat[" Group1: ", BS.intercalate "," (unMetaValue <$> xs), "\n"]
    g2d = case ys of
      [] -> BS.concat [" Group2: !", BS.intercalate "," (unMetaValue <$> xs)]
      _ -> BS.concat[" Group2: ", BS.intercalate "," (unMetaValue <$> ys), "\n"]




-- |If set to all, we need to go through each category in the Table, and create
-- a pairwise comparison for all of the options. Eg. MetaCategory Province,
-- [MetaValue] [AB, BC, SK, QC]
-- Also include one vs. all eg. AB vs. [BC, SK, QC]
-- If the list is only two members long, then one vs. all is not required
-- As A vs. B == B vs. A
getAllPermutations :: Table
                   -> [Comparison]
                   -> MetaCategory
                   -> Set.Set MetaValue
                   -> [Comparison]
getAllPermutations t xs mc mvs = concat [allPairwiseComparisons, ova, xs]
  where
    mxs = Set.toList mvs
    allPairs = getAllListPairs mxs
    allPairwiseComparisons = pairToComparison t mc <$> allPairs
    ova = if length mxs > 2
             then foldl' (getOva t mc) [] mxs
             else []

-- | One vs. all
getOva :: Table
       -> MetaCategory
       -> [Comparison]
       -> MetaValue
       -> [Comparison]
getOva t mc xs v = nc:xs
  where
    nc = Comparison cg1 cg2 cd
    cg1 = filterTable t mc FilterCategory [v]
    cg2 = filterTable t mc AllButCategory [v]
    cd = createComparisonDescription mc [v] mc []

-- |Given a pair of MetaValues, create a comparison of x vs. y
-- This requires wrapping the single value as a list and passing the
-- original table along
pairToComparison :: Table
                 -> MetaCategory
                 -> (MetaValue, MetaValue)
                 -> Comparison
pairToComparison t mc (x, y) = Comparison cg1 cg2 cd
  where
    cg1 = filterTable t mc FilterCategory [x]
    cg2 = filterTable t mc FilterCategory [y]
    cd = createComparisonDescription mc [x] mc [y]


-- |Use the tails function in a list comprehension to generate all needed
-- pairs with no duplicates
getAllListPairs :: [MetaValue]
                -> [(MetaValue, MetaValue)]
getAllListPairs xs = [(x,y) | (x:ys) <- tails xs, y <- ys]

