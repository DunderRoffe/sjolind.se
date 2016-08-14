{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text

import Web.Scotty
import Data.Monoid (mconcat, mempty)
import Data.Acid
import AbsDatabase
import AcidDatabase
import Endpoints

import Network.Wai.Middleware.Static

main :: IO()
main = do
  db <- openLocalStateFrom "db/" emptyDb
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    endpoints db
