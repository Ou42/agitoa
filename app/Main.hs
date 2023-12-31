module Main (main) where

import Options.Applicative

data AppOutput = StdOut | OutputFile !FilePath
  deriving (Show)

data AppOptions = AppOptions
  { withAnswers  :: !Bool
  , maxDigits    :: !Int
  , separation   :: !Int
  , probsInGroup :: !Int
  , output       :: !AppOutput
  , inputFile    :: !FilePath
  }
  deriving (Show)

main :: IO ()
main = arrangerOptions >>= print

arrangerOptions :: IO AppOptions
arrangerOptions =
    execParser $
      info (helper <*> appOptionsParser) $
        progDesc "Arranges arithmetic problems (sums and subtractions) vertically and side by side"
          <> header "arranger - arithmetic arranger version 0.1.0.0"
          <> footer
              "An example app for \
                    \https://www.prborges.com/2023/introduction-to-optparse-applicative"

appOptionsParser :: Parser AppOptions
appOptionsParser =
    AppOptions
        <$> withAnswersParser
        <*> maxDigitsParser
        <*> separationParser
        <*> probsInGroupParser
        <*> outputParser
        <*> inputFileParser

withAnswersParser :: Parser Bool
withAnswersParser =
  switch $
    short 'a'
      <> long "with-answers"
      <> help "Generate problem answers"

maxDigitsParser :: Parser Int
maxDigitsParser =
  option (boundedInt 1 15) $
    short 'd'
      <> long "max-digits"
      <> metavar "Int"
      <> help "Maximum number of digits allowed in a term."
      <> value 4

separationParser :: Parser Int
separationParser =
  option positiveInt $
    short 's'
      <> long "separation"
      <> metavar "Int"
      <> help "Number of spaces between vertically arranged problems."
      <> value 4

probsInGroupParser :: Parser Int
probsInGroupParser =
  option positiveInt $
    short 'g'
      <> long "group-length"
      <> metavar "Int"
      <> help "Number of problems in each horizontal group."
      <> value 5

outputParser :: Parser AppOutput
outputParser =
  option (OutputFile <$> str) $
    short 'o'
      <> long "output-file"
      <> metavar "FILE"
      <> help
          "Output file for the arranged problems. \
          \ If not given, output is written to standard output."
      <> value StdOut

inputFileParser :: Parser FilePath
inputFileParser =
  strArgument $
    metavar "FILE"
      <> help "Input file with one problem per line."

positiveInt :: ReadM Int
positiveInt = auto >>= checkPositive
  where
    checkPositive i
      | i > 0 = pure i
      | otherwise = readerError "Value must be greater than 0"

boundedInt :: Int -> Int -> ReadM Int
boundedInt lower upper = auto >>= checkBounds
  where
    checkBounds i
      | lower <= i && i <= upper = pure i
      | otherwise =
          readerError $
            mconcat [ "Value must be between "
                    , show lower
                    , " and "
                    , show upper
                    ]
