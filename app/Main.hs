{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai (responseLBS, Application)
import Network.HTTP.Types (status200, notFound404)
import Network.Wai.Handler.Warp (run)
import Network.Wai as Wai
import System.IO (FilePath)
import qualified System.Posix.Files as Files
import qualified System.Directory as Dir
import Lib
import qualified Opts as Opts
import qualified Data.ByteString.Char8 as C

fileNotFound :: Response
fileNotFound =
  responseLBS
    notFound404
    [("Content-Type", "text/plain")]
    "File not found"

data FileType
  = File
  | Directory
  | NotFound

getFileType :: FilePath -> IO FileType
getFileType fpath = do
  isFile <- Dir.doesFileExist fpath
  if isFile
  then pure File
  else do
    isDir <- Dir.doesDirectoryExist fpath
    pure (if isDir
            then Directory
            else NotFound)
    
app :: FilePath -> Application
app basepath request respond = do
  let fullPath = basepath <> (C.unpack $ Wai.rawPathInfo request)
  fType <- getFileType fullPath
  respond
    (case fType of
      File ->
        responseFile
          status200
          [("Content-Type", "application/octet-stream")]
          fullPath
          Nothing
      _ -> fileNotFound
    )


main :: IO ()
main = do
  opts <- Opts.runParser
  putStrLn $ "Serving (" <> (Opts.root opts) <> ") on port 7979..."
  run 7979 $ app (Opts.root opts)

