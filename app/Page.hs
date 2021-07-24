{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Page where

import Data.FileEmbed (embedFile)
import Data.ByteString (ByteString)
import Html

waterCss :: ByteString
waterCss = $(embedFile "vendor/water.min.css")




