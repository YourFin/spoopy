-- | 

module Data.MimeType (get, unwrap, MimeType) where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import System.FilePath (FilePath)
import qualified System.FilePath as FilePath
import Data.Default


newtype MimeType = MimeType { unwrap :: B.ByteString }
  deriving (Eq, Show)

instance Default MimeType where
  def = MimeType "application/octet-stream"

get :: FilePath -> MimeType
get fpath =
  case FilePath.takeExtension fpath of
    (char : ext)
      | FilePath.isExtSeparator char ->
        Map.lookup ext extensionMap
         & fromMaybe def
    _ -> def
      
extensionMap :: Map String MimeType
extensionMap =
  fmap MimeType $ Map.fromList 
  -- See: https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Common_types
        [ ("aac", "audio/aac")
        , ("arc", "application/x-freearc")
        , ("avi", "video/x-msvideo")
        , ("bmp", "image/bmp")
        , ("css", "text/css")
        , ("flv", "video/x-flv")
        , ("gif", "image/gif")
        , ("htm", "text/html")
        , ("html", "text/html")
        , ("ico", "image/vnd.microsoft.icon")
        , ("jar", "application/java-archive")
        , ("jpeg", "image/jpeg")
        , ("jpg", "image/jpeg")
        , ("js", "text/javascript")
        , ("json", "application/json")
        , ("jsonld", "application/ld+json")
        , ("m3u8", "application/x-mpegURL")
        , ("mjs", "text/javascript")
        , ("mov", "video/quicktime")
        , ("mp3", "audio/mpeg")
        , ("mp4", "video/mp4")
        , ("mpeg", "video/mpeg")
        , ("oga", "audio/ogg")
        , ("ogg", "video/ogg")
        , ("ogv", "video/ogg")
        , ("ogx", "application/ogg")
        , ("opus", "audio/opus")
        , ("otf", "font/otf")
        , ("png", "image/png")
        , ("pdf", "application/pdf")
        , ("svg", "image/svg+xml")
        , ("swf", "application/x-shockwave-flash")
        , ("tif", "image/tiff")
        , ("tiff", "image/tiff")
        -- should look at this harder, confused easily with typescript
        --, ("ts", "video/mp2t")
        , ("ttf", "font/ttf")
        , ("txt", "text/plain")
        , ("wav", "audio/wav")
        , ("weba", "audio/webm")
        , ("webm", "video/webm")
        , ("webp", "image/webp")
        , ("woff", "font/woff")
        , ("woff2", "font/woff2")
        , ("wmv", "video/x-ms-wmv")
        , ("xhtml", "application/xhtml+xml")
        -- should look into harder, differentiating between
        --  application/html and text/html
        , ("xml", "application/xml")
        , ("3gp", "video/3gpp")
        , ("3g2", "video/3gpp2")
        ]
