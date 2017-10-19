{-# LANGUAGE OverloadedStrings          #-}

module FETSpec where

import Test.Hspec
import FET
import Data.AEq

spec :: Spec
spec =
    describe "Verify p-values of FET tests" $ do
        it "matrix of 2 7 5 3 two tail" $ do
            let fr = fet (FETName "two tail test")
                         (GroupOneA 2)
                         (GroupOneB 7)
                         (GroupTwoA 5)
                         (GroupTwoB 3)
                         TwoTail
            let aeq = pvalue fr ~== (0.153434800493624::Double)
            aeq `shouldBe` True
        it "matrix of 2 7 5 3 one tail" $ do
            let fr = fet (FETName "one tail test")
                         (GroupOneA 2)
                         (GroupOneB 7)
                         (GroupTwoA 5)
                         (GroupTwoB 3)
                         OneTail
            let aeq = pvalue fr ~== (0.11703002879473468::Double)
            aeq `shouldBe` True
        it "matrix of 129 173 152 138 two tail" $ do
            let fr = fet (FETName "two tail")
                         (GroupOneA 129)
                         (GroupOneB 173)
                         (GroupTwoA 152)
                         (GroupTwoB 138)
                         TwoTail
            let aeq = pvalue fr ~== (2.1126884355092402e-2::Double)
            aeq `shouldBe` True
        it "matrix of 129 173 152 138 one tail" $ do
            let fr = fet (FETName "one tail")
                         (GroupOneA 129)
                         (GroupOneB 173)
                         (GroupTwoA 152)
                         (GroupTwoB 138)
                         OneTail
            let aeq = pvalue fr ~== (1.1267280239712145e-2::Double)
            aeq `shouldBe` True
        it "matrix of 222 211 444 555 two tail (ChiSquared)" $ do
            let fr = fet (FETName "two tail Chi Squared")
                         (GroupOneA 222)
                         (GroupOneB 211)
                         (GroupTwoA 444)
                         (GroupTwoB 555)
                         TwoTail
            let aeq = pvalue fr ~== (1.7384585693541843e-2::Double)
            aeq `shouldBe` True
        it "matrix of 222 211 444 555 one tail (ChiSquared)" $ do
            let fr = fet (FETName "one tail ChiSquared")
                         (GroupOneA 222)
                         (GroupOneB 211)
                         (GroupTwoA 444)
                         (GroupTwoB 555)
                         OneTail
            let aeq = pvalue fr ~== (8.692292846770922e-3::Double)
            aeq `shouldBe` True
        it "matrix of 99222 98211 77444 74555 two tail (ChiSquared)" $ do
            let fr = fet (FETName "two tail Chi-Squared")
                         (GroupOneA 99222)
                         (GroupOneB 98211)
                         (GroupTwoA 77444)
                         (GroupTwoB 74555)
                         TwoTail
            let aeq = pvalue fr ~== (4.7092508444146475e-5::Double)
            aeq `shouldBe` True
        it "matrix of 100998 100555 501 555 one tail (ChiSquared)" $ do
            let fr = fet (FETName "one tail Chi-Squared")
                         (GroupOneA 100998)
                         (GroupOneB 100555)
                         (GroupTwoA 501)
                         (GroupTwoB 555)
                         OneTail
            let aeq = pvalue fr ~== (4.193675848378964e-2::Double)
            aeq `shouldBe` True




