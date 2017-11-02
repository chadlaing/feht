{-# LANGUAGE OverloadedStrings #-}

module ComparisonSpec where

import Comparison
import FET
import Test.Hspec
import Test.QuickCheck
import Data.AEq
import qualified Data.Vector.Unboxed as V


spec :: Spec
spec = do
  describe "calculateRatio" $ do
    it "FETResult to a ratio is greater than or equal to -1" $
      property $ \a w x y z -> do
        let fr =
              FETResult
              { groupOneA = w
              , groupOneB = x
              , groupTwoA = y
              , groupTwoB = z
              , pvalue = a
              , fetName = "test"
              }
        let fratio = calculateRatio fr 
        fratio > -1 || fratio ~== -1
    it "FETResult to a ratio is less than or equal to 1" $
      property $ \a w x y z -> do
        let fr =
              FETResult
              { groupOneA = w
              , groupOneB = x
              , groupTwoA = y
              , groupTwoB = z
              , pvalue = a
              , fetName = "test"
              }
        let fratio = calculateRatio fr
        fratio < 1 || fratio ~==1
 
  describe "countCharInVectorByIndices" $ do
    it "Given two indices that match the Char returns the count" $ do
      let v = V.fromList "00abcdefg"
      let indices = [0, 1]
      countCharInVectorByIndices v '0' indices `shouldBe` 2 
    it "Given three indices, two of which match, return the count" $ do
      let v = V.fromList "ty39193883939"
      let indices = [3, 4, 5]
      countCharInVectorByIndices v '9' indices `shouldBe` 2
    it "The vector count should always match the list count" $
      property $ \ x y -> do
        let lcount = length $ filter (== y) x
        let v = V.fromList x
        let allIndices = [0 .. (length x)-1]
        countCharInVectorByIndices v y allIndices `shouldBe` lcount


  describe "bonferroniCorrectComparisonResult" $ do
    it "corrected P-value should be above 0" $
      property $ \ (NonNegative x) (NonNegative y) -> do
        let cr = MkComparisonResult (FETResult 1 1 1 1 x "test") 1.0
        let pv = pvalue . compFET $ bonferroniCorrectComparisonResult y cr
        pv > 0 || pv ~== 0
