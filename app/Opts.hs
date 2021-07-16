-- | 

module Opts (
  Opts(..),
  BasePath,
  runParser,
  unBasePath,
  baseFilePath,
) where

import Options.Applicative
import System.IO (FilePath)
import System.Directory as Dir

data Opts = Opts
  { root :: BasePath
--  , port :: Int
  }
  deriving (Show)

newtype BasePath = BasePath { unBasePath :: FilePath }
  deriving (Show)

baseFilePath :: Opts -> FilePath
baseFilePath = unBasePath . root


parser :: FilePath -> Parser Opts
parser defaultPath = (Opts <$> BasePath)
  <$> strArgument
        (  help "File path that should be served"
        <> value defaultPath
        <> showDefaultWith (const "current working directory")
        <> metavar "path" )

runParser :: IO Opts
runParser = do
  cwd <- Dir.getCurrentDirectory
  execParser (opts cwd)
  where
    opts cwd = info ((parser cwd) <**> helper)
      ( fullDesc
      <> progDesc "Serve a directory over http"
      )
      
