{-# LANGUAGE OverloadedStrings          #-}

module FETSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import FET
import qualified Data.ByteString.Lazy.Char8 as BS


main :: IO ()
main = hspec $ do
    describe "FET of two by two matrix" $ do
        it "Two tail value of matrix of 2, 7 | 5, 3" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 2)
                 (GroupOneB 7)
                 (GroupTwoA 5)
                 (GroupTwoB 3)
                 TwoTail
                `shouldBe`
                   FETResult{groupOneA = 2
                            ,groupOneB = 7
                            ,groupTwoA = 5
                            ,groupTwoB = 3
                            ,pvalue =  0.153434800493624
                            ,fetName = "two-tail test 1"}



        it "One tail value of matrix of 2, 7 | 5, 3" $
                   fet (FETName "two-tail test 1")
                        (GroupOneA 2)
                        (GroupOneB 7)
                        (GroupTwoA 5)
                        (GroupTwoB 3)
                        OneTail
                       `shouldBe`
                          FETResult{groupOneA = 2
                                   ,groupOneB = 7
                                   ,groupTwoA = 5
                                   ,groupTwoB = 3
                                   ,pvalue =  0.11703002879473468
                                   ,fetName = "two-tail test 1"}


        it "Two tail value of matrix of 12, 17 | 15, 13" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 12)
                 (GroupOneB 17)
                 (GroupTwoA 15)
                 (GroupTwoB 13)
                 TwoTail
                `shouldBe`
                   FETResult{groupOneA = 12
                            ,groupOneB = 17
                            ,groupTwoA = 15
                            ,groupTwoB = 13
                            ,pvalue =  0.4308539609661818
                            ,fetName = "two-tail test 1"}



        it "One tail value of matrix of 12, 17 | 15, 13" $
                   fet (FETName "two-tail test 1")
                        (GroupOneA 12)
                        (GroupOneB 17)
                        (GroupTwoA 15)
                        (GroupTwoB 13)
                        OneTail
                       `shouldBe`
                          FETResult{groupOneA = 12
                                   ,groupOneB = 17
                                   ,groupTwoA = 15
                                   ,groupTwoB = 13
                                   ,pvalue =  0.25601654909757166
                                   ,fetName = "two-tail test 1"}

        it "Two tail value of matrix of 129, 173 | 152, 138" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 129)
                 (GroupOneB 173)
                 (GroupTwoA 152)
                 (GroupTwoB 138)
                 TwoTail
                `shouldBe`
                   FETResult{groupOneA = 129
                            ,groupOneB = 173
                            ,groupTwoA = 152
                            ,groupTwoB = 138
                            ,pvalue =  2.1126884355092402e-2
                            ,fetName = "two-tail test 1"}



        it "One tail value of matrix of 129, 173 | 152, 138" $
                   fet (FETName "two-tail test 1")
                        (GroupOneA 129)
                        (GroupOneB 173)
                        (GroupTwoA 152)
                        (GroupTwoB 138)
                        OneTail
                       `shouldBe`
                          FETResult{groupOneA = 129
                                   ,groupOneB = 173
                                   ,groupTwoA = 152
                                   ,groupTwoB = 138
                                   ,pvalue =  1.1267280239712145e-2
                                   ,fetName = "two-tail test 1"}


        it "Two tail ChiSquare of matrix of 222, 211 | 444, 555" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 222)
                 (GroupOneB 211)
                 (GroupTwoA 444)
                 (GroupTwoB 555)
                 TwoTail
                `shouldBe`
                   FETResult{groupOneA = 222
                            ,groupOneB = 211
                            ,groupTwoA = 444
                            ,groupTwoB = 555
                            ,pvalue =  1.7384585693541843e-2
                            ,fetName = "two-tail test 1"}

        it "One tail ChiSquare of matrix of 222, 211 | 444, 555" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 222)
                 (GroupOneB 211)
                 (GroupTwoA 444)
                 (GroupTwoB 555)
                 OneTail
                `shouldBe`
                   FETResult{groupOneA = 222
                            ,groupOneB = 211
                            ,groupTwoA = 444
                            ,groupTwoB = 555
                            ,pvalue =  8.692292846770922e-3
                            ,fetName = "two-tail test 1"}


        it "Two tail ChiSquare of matrix of 99222, 98211 | 77444, 74555" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 99222)
                 (GroupOneB 98211)
                 (GroupTwoA 77444)
                 (GroupTwoB 74555)
                 TwoTail
                `shouldBe`
                   FETResult{groupOneA = 99222
                            ,groupOneB = 98211
                            ,groupTwoA = 77444
                            ,groupTwoB = 74555
                            ,pvalue =  4.7092508444146475e-5
                            ,fetName = "two-tail test 1"}

        it "One tail ChiSquare of matrix of 100998, 100555 | 501, 555" $
            fet (FETName "two-tail test 1")
                 (GroupOneA 100998)
                 (GroupOneB 100555)
                 (GroupTwoA 501)
                 (GroupTwoB 555)
                 OneTail
                `shouldBe`
                   FETResult{groupOneA = 100998
                            ,groupOneB = 100555
                            ,groupTwoA = 501
                            ,groupTwoB = 555
                            ,pvalue =  4.193675848378964e-2
                            ,fetName = "two-tail test 1"}