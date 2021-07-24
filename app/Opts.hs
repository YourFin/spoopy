-- | 

module Opts (
  Opts(..),
  BasePath,
  runParser,
  unBasePath,
  baseFilePath,
) where

import Options.Applicative
import RawFilePath (RawFilePath)
import qualified System.Directory as SlowDir
import Data.String (fromString)

data Opts = Opts
  { root :: BasePath
--  , port :: Int
  }
  deriving (Show)

newtype BasePath = BasePath { unBasePath :: RawFilePath }
  deriving (Show)

baseFilePath :: Opts -> RawFilePath
baseFilePath = unBasePath . root


parser :: RawFilePath -> Parser Opts
parser defaultPath = (Opts <$> BasePath)
  <$> strArgument
        (  help "File path that should be served"
        <> value defaultPath
        <> showDefaultWith (const "current working directory")
        <> metavar "path" )

runParser :: IO Opts
runParser = do
  cwd <- fmap fromString SlowDir.getCurrentDirectory
  execParser (opts cwd)
  where
    opts cwd = info ((parser cwd) <**> helper)
      ( fullDesc
      <> progDesc "Serve a directory over http"
      )
      
