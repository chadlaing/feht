module FET where

import qualified Data.ByteString.Lazy.Char8             as BS
import           Data.Eq
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.List
import           Data.Ord
import           Prelude                                (Double, fromIntegral,
                                                         (+))
import           Statistics.Distribution                (cumulative,
                                                         probability)
import           Statistics.Distribution.Hypergeometric (hypergeometric)
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
-- m is the sum of "has gene" instances, so 9
-- l is the total population size, so 17
-- k is the sum of a particular serotype, so 7
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
  = case mode of
        OneTail -> theResult{pvalue = cumD}
        TwoTail -> theResult{pvalue = twoTailValue}
  where
    cumD = cumulative d $ fromIntegral goa
    probX = probability d goa
    twoTailValue = sum . filter (<= probX) . fmap (probability d) $ [0..k]
    d = hypergeometric m l k
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



