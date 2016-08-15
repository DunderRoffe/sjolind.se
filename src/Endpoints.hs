{-# LANGUAGE OverloadedStrings #-}
module Endpoints where

import Web.Scotty
import Data.Acid
import Data.Text

import AbsDatabase
import AcidDatabase
import Auth
import CommonScotty

import Endpoints.ProjectEndpoints
import Endpoints.AuthEndpoints
import Endpoints.StartupEndpoints

import Control.Monad.IO.Class (liftIO)
import Control.Monad (liftM, when, unless)

endpoints :: AcidState Database -> ScottyM ()
endpoints db = do
    -- Startup
    get  "" $ do
         e <- liftIO $ query db IsEmpty
         unless e next
         presentStartupPage
    post "" $ do
         e <- liftIO $ query db IsEmpty
         unless e next
         initDB db

    -- Project
    get  "" $ do
         liftIO $ print "lel"
         presentProject db "" ""
    get  "/:project/" $ do
         projName <- param "project"
         presentProject db projName ""
    get  "/:project/:post" $ do
         projName <- param "project"
         postName <- param "post"
         presentProject db projName postName

    get  "/:project/file/:filename" $ getProjectFile db
    post "/:project/file/" $ do 
         uri <- getServerUri db
         ifAuthenticated uri $ updateProjectFile db

    post "/:project/post" $  do
         uri <- getServerUri db
         ifAuthenticated uri $ updateProjectPost db

    -- Auth
    get  (literal (unpack indieAuthRoute)) $ do
         uri <- getServerUri db
         authenticate uri

