{-# LANGUAGE OverloadedStrings #-}

module ComparisonSpec where

import Comparison
import FET
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "calculateRatio" $ do
    it "FETResult to a ratio is greater than 0" $
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
        calculateRatio fr >= 0
