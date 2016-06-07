{-# LANGUAGE OverloadedStrings #-}
module Constants where

import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

serverUri :: LT.Text
serverUri = "http://localhost:3000"

melinks :: [T.Text]
melinks = ["https://github.com/dunderroffe"]
