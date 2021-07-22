{-# LANGUAGE TemplateHaskell #-}

module Opts (
  Opts(..),
  BasePath,
  PortNum,
  askBasePath,
  askPortNum,
  runIO,
  runPure
) where

import Data.Function ((&))
import Options.Applicative
import System.Posix.ByteString (RawFilePath)
import System.Posix.Directory.ByteString as Dir
import Polysemy
import Polysemy.Reader (runReader, Reader, asks)

-- Types

data Opts = Opts
  { root :: RawFilePath
  , port :: Int
  }
  deriving (Show)

newtype BasePath = BasePath { unBasePath :: RawFilePath }
  deriving (Show, Eq)

newtype PortNum = PortNum { unPortNum :: Int }
  deriving (Show, Eq)

-- Command line parser

parser :: RawFilePath -> Parser Opts
parser defaultPath = Opts
  <$> strArgument
        (  help "File path that should be served"
        <> value defaultPath
        <> showDefaultWith (const "current working directory")
        <> action "directory" -- Autocomplete on directory names
        <> metavar "path" )
  <*> option (auto @Int)
        (  short 'p'
        <> long "port"
        <> help "Port to serve on"
        <> value 7979
        <> showDefault
        <> metavar "port"
        )

parserInfo :: RawFilePath -> ParserInfo Opts
parserInfo defaultPath =
  info ((parser defaultPath) <**> helper)
                    ( fullDesc
                      <> progDesc "Serve a directory over http"
                    )

-- Effects

askBasePath :: Member (Reader BasePath) r => Sem r RawFilePath
askBasePath = asks unBasePath

askPortNum :: Member (Reader PortNum) r => Sem r Int
askPortNum = asks unPortNum

runIO :: Member (Embed IO) r =>
  Sem ((Reader BasePath) ': (Reader PortNum) ': r) a -> Sem r a
runIO hasReaders = do
  cwd <- embed Dir.getWorkingDirectory
  opts <- embed $ execParser $ parserInfo cwd
  hasReaders
   & runReader (BasePath $ root opts)
   & runReader (PortNum $ port opts)

runPure :: Opts -> Sem ((Reader BasePath) ': (Reader PortNum) ': r) a -> Sem r a
runPure opts hasReaders =
  hasReaders
   & runReader (BasePath $ root opts)
   & runReader (PortNum $ port opts)
