module Main where

import Data.Function ((&))
import Network.Wai (responseLBS, Application)
import Network.HTTP.Types (status200, notFound404)
import Network.Wai.Handler.Warp (run)
import Network.Wai as Wai
import System.IO (FilePath)
import Control.Exception (try)
import qualified System.Posix.Files as Files
import qualified System.Directory as Dir
import Lib
import qualified Opts as Opts
import qualified Data.ByteString.Char8 as C
import qualified System.FilePath as FP
import qualified System.Posix.Files as Posix
import System.IO.Error (IOError)
import qualified Data.List as List
import qualified Data.Either as Either
import Data.MimeType (MimeType)
import qualified Data.MimeType as MimeType

fileNotFound :: Response
fileNotFound =
  responseLBS
    notFound404
    [("Content-Type", "text/plain")]
    "File not found"

data FileType
  = File MimeType
  | Directory
  | NotFound

getFileType :: Opts.BasePath -> FilePath -> IO FileType
getFileType basepath fpath = do
  isShareable <- isShareable basepath fpath
  isFile <- Dir.doesFileExist fpath
  if | not isShareable -> pure NotFound
     | isFile          -> pure . File $ MimeType.get fpath
     | otherwise -> do
         isDir <- Dir.doesDirectoryExist fpath
         pure (if isDir
                 then Directory
                 else NotFound)

isShareable :: Opts.BasePath -> FilePath -> IO Bool
isShareable basePath checkPath = do
  let base = Opts.unBasePath basePath
  canBasePath <- Dir.canonicalizePath base
  canCheckPath <- Dir.canonicalizePath checkPath
  fileMode <- safeFileMode checkPath
  pure $ (List.isPrefixOf canBasePath canCheckPath)
         && (isOtherReadable fileMode)
    where
      safeFileMode fpath =
        Posix.getFileStatus fpath
          -- default to no permissions when the file doesn't exist
          & fmap Posix.fileMode
          & try @IOError
          & fmap (Either.fromRight Posix.nullFileMode)
      isOtherReadable fileMode =
        (Posix.intersectFileModes Posix.otherReadMode fileMode)
          == Posix.otherReadMode
    
app :: Opts.BasePath -> Application
app basepath request respond = do
  let fullPath = (Opts.unBasePath basepath) <> (C.unpack $ Wai.rawPathInfo request)
  fType <- getFileType basepath fullPath
  respond
    (case fType of
      File mimeType ->
        responseFile
          status200
          [("Content-Type", MimeType.unwrap mimeType)]
          fullPath
          Nothing
      _ -> fileNotFound
    )


main :: IO ()
main = do
  opts <- Opts.runParser
  putStrLn $ "Serving (" <> (Opts.baseFilePath opts) <> ") on port 7979..."
  run 7979 $ app (Opts.root opts)

