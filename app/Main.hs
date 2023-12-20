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
    execParser $ info appOptionsParser mempty

appOptionsParser :: Parser AppOptions
appOptionsParser =
    AppOptions
        <$> withAnswersParser
        <*> maxDigitsParser
        <*> separationParser
        <*> probsInGroupParser
        <*> outputParser
        <*> inputFileParser
    where
        withAnswersParser :: Parser Bool
        withAnswersParser =
          switch $
            short 'a'
              <> long "with-answers"
              <> help "Generate problem answers"
        maxDigitsParser :: Parser Int
        maxDigitsParser =
          option auto $
            short 'd'
              <> long "max-digits"
              <> metavar "Int"
              <> help "Maximum number of digits allowed in a term."
              <> value 4
        separationParser :: Parser Int
        separationParser = pure 4
        probsInGroupParser :: Parser Int
        probsInGroupParser = pure 5
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
        inputFileParser = pure "input.txt"
