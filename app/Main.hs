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
appOptionsParser = undefined
