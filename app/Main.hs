module Main where

import Data.Function ((&))
import Network.Wai (responseLBS)
import Network.HTTP.Types (status200, notFound404)
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai as Wai
import RawFilePath (RawFilePath)
import Control.Exception (try, bracket)
import qualified RawFilePath.Directory as Dir
import qualified System.Directory as SlowDir
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
import qualified System.Posix.IO.ByteString as BPosix

fileNotFound :: Wai.Response
fileNotFound =
  responseLBS
    notFound404
    [("Content-Type", "text/plain")]
    "File not found"

data FileType
  = File MimeType
  | Directory
  | NotFound

getFileType :: Opts.BasePath -> RawFilePath -> IO FileType
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

isShareable :: Opts.BasePath -> RawFilePath -> IO Bool
isShareable basePath checkPath = do
  let base = Opts.unBasePath basePath
  canBasePath <- SlowDir.canonicalizePath $ C.unpack base
  canCheckPath <- SlowDir.canonicalizePath $ C.unpack checkPath
  fileMode <- safeFileMode checkPath
  pure $ (List.isPrefixOf canBasePath canCheckPath)
         && (isOtherReadable fileMode)
    where
      safeFileMode fpath =
        getFileStatus fpath
          -- default to no permissions when the file doesn't exist
          & fmap Posix.fileMode
          & try @IOError
          & fmap (Either.fromRight Posix.nullFileMode)
      isOtherReadable fileMode =
        (Posix.intersectFileModes Posix.otherReadMode fileMode)
          == Posix.otherReadMode

getFileStatus :: RawFilePath -> IO Posix.FileStatus
getFileStatus path =
  bracket (BPosix.openFd
              path
              BPosix.ReadOnly
              Nothing
              BPosix.OpenFileFlags
                { BPosix.append = False
                , BPosix.exclusive = False
                , BPosix.noctty = False
                , BPosix.nonBlock = False
                , BPosix.trunc = False
                }
          )
          BPosix.closeFd
          Posix.getFdStatus
    
app :: Opts.BasePath -> Wai.Application
app basepath request respond = do
  let fullPath = (Opts.unBasePath basepath) <> (Wai.rawPathInfo request)
  fType <- getFileType basepath fullPath
  respond
    (case fType of
      File mimeType ->
        Wai.responseFile
          status200
          [("Content-Type", MimeType.unwrap mimeType)]
          (C.unpack fullPath)
          Nothing
      _ -> fileNotFound
    )


main :: IO ()
main = do
  opts <- Opts.runParser
  C.putStrLn $ "Serving (" <> (Opts.baseFilePath opts) <> ") on port 7979..."
  run 7979 $ app (Opts.root opts)

