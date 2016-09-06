{-# LANGUAGE OverloadedStrings #-}
module Endpoints where

import Web.Scotty
import Data.Acid
import Data.Text (unpack)

import AbsDatabase
import AcidDatabase
import Auth

import Endpoints.Common
import Endpoints.ProjectEndpoints
import Endpoints.AuthEndpoints
import Endpoints.StartupEndpoints

import Webmentions

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)

import Network.HTTP.Types

endpoints :: AcidState Database -> ScottyM ()
endpoints db = do

    -- Startup -- Will only be called if the database is empty
    get  "" $ do
         e <- liftIO $ query db IsEmpty
         if e then presentStartupPage
              else next

    post "" $ do
         e <- liftIO $ query db IsEmpty
         if e then initDB db
              else next

    -- Webmentions
    post "webmentions" $ do
         source <- param "source"
         target <- param "target"

         liftIO $ processWebmentionAsync (source, target) db
         status accepted202

    -- Project -- Present and update projects
    get  "" $ presentProject db "" ""

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

    -- IndieAuth -- Handle authentication calls from IndieAuth
    get  (literal (unpack indieAuthRoute)) $ do
         uri <- getServerUri db
         authenticate uri

