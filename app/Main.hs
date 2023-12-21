module Main where

import Main.Repl (repl)
import Control.Monad (forM_)
import Options.Applicative
import System.Directory

-- | Command line arguments.
data Args = Args
  { argDirectory :: Maybe String
    -- ^ @[-d|--directory]@ Set the working directory.
  }
  deriving (Show)

-- | Command line argument specification.
argParser :: Parser Args
argParser = Args
  <$> (optional $ strOption
        ( long "directory"
       <> short 'd'
       <> metavar "DIR"
       <> help "Set the working directory" ))

main :: IO ()
main = do
  -- Parse command line arguments.
  args <- execParser $
    info (argParser <**> helper) fullDesc

  -- Set the current directory, if applicable.
  forM_ (argDirectory args) setCurrentDirectory

  -- Start the repl.
  repl
