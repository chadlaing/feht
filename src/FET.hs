module FET where

import qualified Data.ByteString.Lazy.Char8             as BS
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.List
import           Data.Ord
import           Prelude                                (Double, fromIntegral,
                                                         (*), (+), (-), (/),
                                                         (^), otherwise, (&&))
import           Statistics.Distribution                (complCumulative,
                                                         cumulative,
                                                         probability)
import           Statistics.Distribution.ChiSquared
import           Statistics.Distribution.Hypergeometric
import           Text.Show

--the Fisher's exact test can be computed from the hypergeometric distribution
--eg. test whether the presence / absence of a gene is statistically significant
--between two bacterial groups
--           Group 1 | Group 2
--          -------------------
-- has gene |   2    |    7
-- no gene  |   5    |    3
--          -------------------
-- the hypergeometric distribution is calculated in Statistics.Distribution.Hypergeometric
-- hypergeometric = m l k
-- From the package description: The parameters of the distribution describe
-- k elements chosen from a population of l, with m elements of one type,
-- and l-m of the other (all are positive integers).
-- In our case:
-- m is the sum of "has factor" instances, so 9
-- l is the total population size, so 17
-- k is the sum of a particular group, so 7 for Group 1
-- let d = distribution 9 17 7
-- This gives us the distribution, but we need to test the probability
-- that the presence of this gene is statistically different, using the two-tail
-- approach.
-- The one-tail probability in our example of Group 1 having 2 members with the
-- gene can be tested using the cumulative distribution function from
-- Statistics.Distribution, our previously calculated distribution "d", and
-- the number of "successes" (x), which is 2.
-- let oneTail = cumulative d 2 = 0.11703002879473468
-- However, we have no previous expectation for this group to have or not have
-- this gene, so what we really want is the two-tailed test.
-- Please see http://www.pmean.com/11/Fishers.html for an excellent,
-- detailed description.
-- In short, we need to sum all of the probabilities that are smaller than or
-- equal to the probability of the observed table (eg. x = 2 in our example).
-- The twoTailValue is calculated by generating all of the probabilities for
-- our sample, filtering the ones that have a probability too large, and
-- summing the remaining values.
--
-- For a total number of observations greater than 1000, we use the Chi Square
-- test, as the FET returns NaN.
-- ChiSqr statistic  for
--           Group 1 | Group 2
--          -------------------
-- has gene |   2    |    7
-- no gene  |   5    |    3
--          -------------------
-- is [(2*3)-(5*7)]^2 * (2+7+5+3) / (2+7)(5+3)(7+3)(2+5)
-- With 1 degree of freedom, we can calculate the Chi Square distribution
-- with chiSquared 1 from Statistics.Distribution
-- To determine the P value of a Chi Square statistic, we use the
-- complCumulative function and the
-- Chi Square distribution with 1 degree of freedom.
-- For Chi Square, the one-tail is simply half the two-tail
-- Statistics.Test.ChiSquared only return "Significant" or "NotSignificant"
-- and we wanted the actual P-value, hence the function.
-- Likewise there is no Statistics.Test.FET


newtype GroupOneA = GroupOneA { unGroupOneA :: Int } deriving (Eq, Show)
newtype GroupOneB = GroupOneB { unGroupOneB :: Int } deriving (Eq, Show)
newtype GroupTwoA = GroupTwoA { unGroupTwoA :: Int } deriving (Eq, Show)
newtype GroupTwoB = GroupTwoB { unGroupTwoB :: Int } deriving (Eq, Show)
newtype PValue = PValue {unPValue :: Double} deriving (Eq, Show)
newtype FETName = FETName {unFETName :: BS.ByteString} deriving(Eq, Show)
data FETMode = OneTail | TwoTail deriving (Eq,Show)

data FETResult = FETResult{groupOneA :: Int
                          ,groupOneB :: Int
                          ,groupTwoA :: Int
                          ,groupTwoB :: Int
                          ,pvalue    :: Double
                          ,fetName   :: BS.ByteString} deriving (Eq, Show)

fet :: FETName
    -> GroupOneA
    -> GroupOneB
    -> GroupTwoA
    -> GroupTwoB
    -> FETMode
    -> FETResult
fet (FETName fName) (GroupOneA goa) (GroupOneB gob) (GroupTwoA gta) (GroupTwoB gtb) mode 
  | l > 0 && goa >= 0 && gob >= 0 && gta >= 0 && gtb >= 0 =
    case mode of
        OneTail -> theResult{pvalue = cumD}
        TwoTail -> theResult{pvalue = twoTailValue}
  | otherwise = theResult
  where
    cumD = if l >= 1000
            then chiP / 2
            else cumulative d $ fromIntegral goa
    probX = probability d goa
    twoTailValue = if l >= 1000
                      then chiP
                      else sum . filter (<= probX) . fmap (probability d) $ [0..k]
    d = hypergeometric m l k
    chiP = complCumulative d' $ chiSquare goa gob gta gtb
    d' = chiSquared 1
    m = goa + gta
    l = m + gob + gtb
    k = goa + gob
    theResult = FETResult{groupOneA = goa
                         ,groupOneB = gob
                         ,groupTwoA = gta
                         ,groupTwoB = gtb
                         ,pvalue = 1
                         ,fetName = fName
                         }

chiSquare :: Int
          -> Int
          -> Int
          -> Int
          -> Double
chiSquare a' b' c' d' = (x' * x' * (a + b + c + d)) /
                        ((a+b) * (c+d) * (b+d) * (a+c))
  where
    x' = (a * d) - (b * c)
    a = fromIntegral a'
    b = fromIntegral b'
    c = fromIntegral c'
    d = fromIntegral d'
