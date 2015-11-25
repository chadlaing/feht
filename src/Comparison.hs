{-# LANGUAGE DeriveGeneric #-}

module Comparison where

import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char
import           Data.Eq
import           Data.Foldable
import           Data.Function
import           Data.Hashable
import qualified Data.HashMap.Strict        as M
import           Data.List                  (filter, (\\))
import           Data.Maybe
import           Data.Ord
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics               (Generic)
import           FET
import           Table
import           Prelude                    (fromIntegral, length, (++), (-),
                                             (/))
import           Text.Show


data Comparison = Comparison{compGroup1  :: Metadata
                            ,compGroup2  :: Maybe Metadata
                            ,filterGroup :: Maybe Metadata
                            }deriving (Show, Eq, Ord, Generic)

instance Hashable Comparison


--we want a HashMap of all metadata,
--eg. Serotype -> [O157:H7, O26:H11]; Location -> [Canada, Sweden];
--Source -> [Bovine, Human, Environment]
--this function should iterate through all of the keys are create a comparison
--based on all the gene data for each value of each key, plus vs. all and combined vs. all
--eg. for Serotype
--  O157:H7 vs. O26:H11
--  O157:H7 vs. all else
--  O26:H11 vs. all else
-- (O26:H11 + O157:H7) vs. all else
-- do the same for all other metadata entries in the HashMap.
-- Second Level analyses:
-- Filter based on every term of one Metadata type and filter based on the others
-- eg.
-- Filter the data for only entries containing O157:H7
-- then perform:
-- Bovine vs. all
-- Bovine vs. Human ... etc. for all combos, the same as above for Serotype.
-- then for the Location, while still filtered for O157:H7
-- Canada vs. All
-- Sweden vs. All ... etc. for all combos.
-- This should be dynamic for as many Metadata categories and values that are entered
calculateMetadata :: M.HashMap GenomeName Table
                  -> M.HashMap GeneName (V.Vector Char)
                  -> [Metadata]
                  -> M.HashMap Comparison [FETResult]
calculateMetadata hm vc = foldl' compareGroups M.empty
  where
    compareGroups :: M.HashMap Comparison [FETResult]
                  -> Metadata
                  -> M.HashMap Comparison [FETResult]
    compareGroups hm' x = M.insert currentComparison fetResults hm'
      where
        currentComparison = Comparison{compGroup1 = x
                                      ,compGroup2 = Nothing
                                      ,filterGroup = Nothing}
        fetResults = M.foldlWithKey' getFET [] vc
          where
            goMetaInfo = filterMetadataInfo [x] hm
            goColumnList = getColumnList goMetaInfo
            allColumnList = getColumnList hm
            --due to filtering the non-column containing metadata, the
            --column list is not sequential with number of items
            gtColumnList = allColumnList \\ goColumnList
            got = M.size goMetaInfo
            tl = M.size hm
            getFET :: [FETResult]
                   -> GeneName
                   -> V.Vector Char
                   -> [FETResult]
            getFET fr gName vc' = theResult:fr
              where
                goa = countCharInVectorByIndices vc' '1' goColumnList
                gta = countCharInVectorByIndices vc' '1' gtColumnList
                gob = got - goa
                gtb = tl - goa - gob - gta
                theResult = fet (FETName $ unGeneName gName) (GroupOneA goa) (GroupOneB gob) (GroupTwoA gta)
                    (GroupTwoB gtb) TwoTail


formatComparisonAsTable :: M.HashMap Comparison [FETResult] -> [BS.ByteString]
formatComparisonAsTable = M.foldlWithKey' formatFETResult []
  where
    formatFETResult :: [BS.ByteString]
                    -> Comparison
                    -> [FETResult]
                    -> [BS.ByteString]
    formatFETResult xs c fr = newComparison ++ xs
      where
        columnHeaders = BS.pack "Name\tGroupOneA\tGroupOneB\tGroupTwoA\tGroubTwoB\tpValue"
        newComparison = getHeader c:columnHeaders:xs'
        getHeader :: Comparison -> BS.ByteString
        getHeader c' = BS.intercalate (BS.singleton '\t')
                         [fromMetadata $ compGroup1 c'
                         ,maybe (BS.pack "all") fromMetadata $ compGroup2 c'
                         ,maybe (BS.pack "no filter") fromMetadata $ filterGroup c'
                         ]
        xs' = foldl' getFetLine [] fr
          where
            getFetLine :: [BS.ByteString] -> FETResult -> [BS.ByteString]
            getFetLine xs'' fr' = newResult:xs''
              where
                newResult = BS.intercalate (BS.singleton '\t')
                              [fetName fr'
                              ,BS.pack . show . groupOneA $ fr'
                              ,BS.pack . show . groupOneB $ fr'
                              ,BS.pack . show . groupTwoA $ fr'
                              ,BS.pack . show . groupTwoB $ fr'
                              ,BS.pack . show . pvalue $ fr'
                              ]


filterComparisonsByPValue :: M.HashMap Comparison [FETResult]
                          -> M.HashMap Comparison [FETResult]
filterComparisonsByPValue = M.foldlWithKey' isSignificant M.empty
  where
    isSignificant :: M.HashMap Comparison [FETResult]
                  -> Comparison
                  -> [FETResult]
                  -> M.HashMap Comparison [FETResult]
    isSignificant hm k fr = M.insert k filteredList hm
      where
        filteredList = filter (\x -> pvalue x < correctedCutoff) fr
        correctedCutoff = 0.05 / fromIntegral numberOfComparisons
        numberOfComparisons = length fr



