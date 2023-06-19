-- | Command-line options parsing
module OptParse
  ( Options (..),
    SingleInput (..),
    SingleOutput (..),
    parse,
  )
where

import Data.Maybe (fromMaybe)
import Options.Applicative

------------------------------------------------

-- * Our command-line options model

-- | Model
data Options
  = ConvertSingle SingleInput SingleOutput
  | ConvertDir FilePath FilePath
  deriving (Show)

-- | A single input source
data SingleInput
  = Stdin
  | InputFile FilePath
  deriving (Show)

-- | A single output sink
data SingleOutput
  = Stdout
  | OutputFile FilePath
  deriving (Show)

------------------------------------------------

-- * Parser

-- | Parse command-line options
parse :: IO Options
parse = execParser opts

opts :: ParserInfo Options
opts = info (helper <*> pOptions) desc
  where
    desc = fullDesc <> descHeader <> descBody
    descHeader = header "HsBlogGen - a static blog generator"
    descBody = progDesc "Convert markup files or directories to html"

-- | Parser for all options
pOptions :: Parser Options
pOptions = subparser $ pSingleCommand <> pDirCommand

pSingleCommand :: Mod CommandFields Options
pSingleCommand = command "convert" $ info (helper <*> pConvertSingle) desc
  where
    desc = progDesc "Convert a single markup source to html"

pDirCommand :: Mod CommandFields Options
pDirCommand = command "convert-dir" $ info (helper <*> pConvertDir) desc
  where
    desc = progDesc "Convert a directory of markup files to html"

------------------------------------------------

-- * Single source to sink conversion parser

-- | Parser for single source to sink option
pConvertSingle :: Parser Options
pConvertSingle = ConvertSingle <$> pSingleInput <*> pSingleOutput

-- | Parser for single input source
pSingleInput :: Parser SingleInput
pSingleInput = fromMaybe Stdin <$> optional pInputFile

-- | Parser for single output sink
pSingleOutput :: Parser SingleOutput
pSingleOutput = fromMaybe Stdout <$> optional pOutputFile

-- | Input file parser
pInputFile :: Parser SingleInput
pInputFile = InputFile <$> parser
  where
    parser = strOption optFields
    optFields = long "input" <> short 'i' <> metavar "FILE" <> help "Input file"

-- | Output file parser
pOutputFile :: Parser SingleOutput
pOutputFile = OutputFile <$> parser
  where
    parser = strOption optFields
    optFields = long "output" <> short 'o' <> metavar "FILE" <> help "Output file"

------------------------------------------------

-- * Directory conversion parser

pConvertDir :: Parser Options
pConvertDir = ConvertDir <$> pInputDir <*> pOutputDir

-- | Parser for input directory
pInputDir :: Parser FilePath
pInputDir = strOption optFields
  where
    optFields = long "input" <> short 'i' <> metavar "DIRECTORY" <> help "Input directory"

-- | Parser for output directory
pOutputDir :: Parser FilePath
pOutputDir = strOption optFields
  where
    optFields = long "output" <> short 'o' <> metavar "DIRECTORY" <> help "Output directory"
