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
        withAnswersParser = pure False
        maxDigitsParser :: Parser Int
        maxDigitsParser = pure 4
        separationParser :: Parser Int
        separationParser = pure 4
        probsInGroupParser :: Parser Int
        probsInGroupParser = pure 5
        outputParser :: Parser AppOutput
        outputParser = pure StdOut
        inputFileParser :: Parser FilePath
        inputFileParser = pure "input.txt"
