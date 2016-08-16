{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Acid
import Web.Scotty
import Network.Wai.Middleware.Static

import AbsDatabase
import AcidDatabase
import Endpoints

main :: IO()
main = do
  db <- openLocalStateFrom "db/" emptyDb
  scotty 3000 $ do
    middleware $ staticPolicy (noDots >-> addBase "static")
    endpoints db
