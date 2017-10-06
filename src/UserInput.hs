module UserInput where

import           Options.Applicative
import           Data.Semigroup      ((<>))

--for command line processing using optparse-applicative
data UserInput = UserInput
    {metafile   :: FilePath
    ,datafile   :: FilePath
    ,one        :: String
    ,two        :: String
    ,delimiter  :: String
    ,mode       :: UserMode
    ,correction :: String
    } deriving (Show, Eq, Read)

data UserMode = Binary | Snp deriving (Eq, Show, Read)

parseUserMode :: ReadM UserMode
parseUserMode = eitherReader $ \arg ->
   if arg == "snp"
     then Right Snp
     else if arg == "binary"
             then Right Binary
             else Left "Incorrect option type"

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
  <*> strOption
      (long "delimiter"
      <> short 'l'
      <> metavar "[',', '\\t' ...], DEFAULT=','"
      <> value ","
      <> help "Delimiter used for both the metadata and data file")
  <*> option parseUserMode
      (long "mode"
      <> short 'o'
      <> metavar "['binary', 'snp'], DEFAULT='binary'"
      <> value Binary
      <> help "Mode for program data; either 'binary' or 'snp'")
  <*> strOption
      (long "correction"
      <> short 'c'
      <> metavar "['none', 'bonferroni'], DEFAULT='bonferroni'"
      <> value "bonferroni"
      <> help "Multiple-testing correction to apply"
      )

opts :: ParserInfo UserInput
opts = info (feht <**> helper)
  (fullDesc
  <> progDesc "Predictive marker discovery for groups; binary data, genomic data (single nucleotide variants), and arbitrary character data."
  <> header "feht - predictive marker discovery")

