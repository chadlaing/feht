{-# LANGUAGE OverloadedStrings #-}

module ComparisonSpec where

import Comparison
import FET
import Test.Hspec
import Test.QuickCheck
import Data.AEq

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
