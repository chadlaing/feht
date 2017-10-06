module UserInput where

import           Options.Applicative
import           Data.Semigroup      ((<>))
import qualified Data.ByteString.Lazy.Char8 as BS


-- |For command line processing using optparse-applicative
-- Use custom data types where possible.
data UserInput = UserInput
    {metafile   :: FilePath
    ,datafile   :: FilePath
    ,one        :: String
    ,two        :: String
    ,delimiter  :: Char
    ,mode       :: UserMode
    ,correction :: Correction
    } deriving (Show, Eq, Read)

data UserMode = Binary | Snp deriving (Eq, Show, Read)
data Correction = Bonferroni | None deriving (Eq, Show, Read)

-- |The multiple testing correction types as their own data type.
parseCorrection :: ReadM Correction
parseCorrection = eitherReader $ \arg ->
  case arg of
    "bonferroni" -> Right Bonferroni
    "none" -> Right None
    _ -> Left "Incorrect multiple testing type selected"


-- |Need to create a custom option type so that we can return more than a String.
-- The mode of the program based on the user data type is either Snp or Binary.
parseUserMode :: ReadM UserMode
parseUserMode = eitherReader $ \arg ->
  case arg of
    "snp" -> Right Snp
    "binary" -> Right Binary
    _ -> Left "Incorrect data mode type."


feht :: Parser UserInput
feht = UserInput
  <$> strOption
      (long "metafile"
      <> short 'm'
      <> metavar "FILE"
      <> help "File of metadata information")
  <*> strOption
      (long "datafile"
      <> short 'd'
      <> metavar "FILE"
      <> help "File of binary or single-nucleotide variant data")
  <*> strOption
      (long "one"
      <> metavar "Group1Name Group1Item Group1Item Group1Item"
      <> help "Group1 column name, followed by optional Group1 labels to include as part of the group")
  <*> strOption
      (long "two"
      <> metavar "Group2Name Group2Item Group2Item Group2Item"
      <> help "Group2 column name, followed by optional Group2 labels to include as part of the group")
  <*> option auto
      (long "delimiter"
      <> short 'l'
      <> metavar "[',', '\\t' ...], DEFAULT=','"
      <> value ','
      <> help "Delimiter used for both the metadata and data file")
  <*> option parseUserMode
      (long "mode"
      <> short 'o'
      <> metavar "['binary', 'snp'], DEFAULT='binary'"
      <> value Binary
      <> help "Mode for program data; either 'binary' or 'snp'")
  <*> option parseCorrection
      (long "correction"
      <> short 'c'
      <> metavar "['none', 'bonferroni'], DEFAULT='bonferroni'"
      <> value Bonferroni
      <> help "Multiple-testing correction to apply"
      )

opts :: ParserInfo UserInput
opts = info (feht <**> helper)
  (fullDesc
  <> progDesc "Predictive marker discovery for groups; binary data, genomic data (single nucleotide variants), and arbitrary character data."
  <> header "feht - predictive marker discovery")


