-- | 

module Opts where

import Options.Applicative
import System.IO (FilePath)
import System.Directory as Dir

data Opts = Opts
  { root :: FilePath
--  , port :: Int
  }
  deriving (Show)

parser :: FilePath -> Parser Opts
parser defaultPath = Opts
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
      
