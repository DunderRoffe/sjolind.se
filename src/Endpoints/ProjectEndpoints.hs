{-# LANGUAGE OverloadedStrings #-}
module Endpoints.ProjectEndpoints where

import Data.Acid
import Data.Text
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as LT (fromStrict)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as LBS (fromStrict, toStrict)
import qualified Data.Map as Map

import Page
import Project
import Auth
import AcidDatabase
import AbsDatabase
import CommonScotty

import Network.HTTP.Types
import Network.Wai.Parse

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (liftM, when, unless)

import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

projectEndpoints :: AcidState Database -> S.ScottyM ()
projectEndpoints db = do 
    S.get "" $ viewProj db "" ""

    S.get "/:project/" $ do
          projName <- S.param "project"
          viewProj db projName ""

    S.get "/:project/:post" $ do
          projName    <- S.param "project"
          postHeading <- S.param "post"
          viewProj db projName postHeading

    S.get "/:project/file/:filename" $ do
          projName    <- S.param "project"
          filename    <- S.param "filename"
          mproj <- liftIO $ query db (GetProject projName)
          case mproj of
            Nothing      -> S.status badRequest400
            Just project -> do
              let fm = projectFilesMap project
              case Map.lookup filename fm of
                Nothing                -> S.status badRequest400
                Just (File _ fileData) -> do
                  S.setHeader "Content-Disposition" $ LT.fromStrict $TE.decodeUtf8 filename
                  S.raw $ LBS.fromStrict fileData

    S.post "/:project/post" $ do
          uri <- getServerUri db
          authenticated <- isAuthenticated uri
          if authenticated
            then do
              projectName    <- S.param "project"
              (Just project) <- liftIO $ query db (GetProject projectName)
              contentType    <- S.header "Content-Type"
              authorName     <- S.param "author-name"
              authorImage    <- S.param "author-image"
              authorUri      <- S.param "author-uri"
              heading        <- S.param "heading"
              date           <- S.param "date"
              content        <- S.param "content"

              let author    = Author authorName authorImage authorUri
                  post      = Post author heading date content []
                  postsMap' = Map.insert heading post (projectPostsMap project)
                  project'  = project {projectPostsMap = postsMap'}

              liftIO $ update db (UpdateProject project')
              S.redirect $ LT.fromStrict uri

            `S.rescue` (\msg -> do
              S.status badRequest400
              S.text msg)

            else
              S.status unauthorized401

    S.post "/:project/file/" $ do
          uri <- getServerUri db
          authenticated <- isAuthenticated uri
          if authenticated
            then do
              projectName        <- S.param "project"
              (Just project)     <- liftIO $ query db (GetProject projectName)
              ((_, fileinfo): _) <- S.files

              let filesMap' = Map.insert (fileName fileinfo) file (projectFilesMap project)
                  file      = File (fileName fileinfo) (LBS.toStrict (fileContent fileinfo))
                  project'  = project {projectFilesMap = filesMap'}

              liftIO $ update db (UpdateProject project')
              liftIO $ print $ mconcat ["file ", fileName fileinfo, " added"]
              S.redirect $ LT.fromStrict uri

            `S.rescue` (\msg -> do
              S.status badRequest400
              S.text msg)
            else
              S.status unauthorized401

viewProj :: AcidState Database -> Text -> Text -> S.ActionM ()
viewProj db "" _ = do
    (Project n _ _ _ _) <- liftIO $ query db GetMainProject
    viewProj db n ""

viewProj db projName postHeading = do
    mproj <- liftIO $ query db (GetProject projName)
    mpost <- liftIO $ query db (GetPost projName postHeading)
    uri <- getServerUri db
    case mproj of
        Nothing   -> S.redirect $ LT.fromStrict uri
        Just proj -> do
            cookieCode <- liftM (fromMaybe "") $ SC.getCookie $ getCookieName uri
            let authenticated = cookieCode /= ""
            vs  <- liftIO $ query db GetVerificationUris
            S.html $ renderCore vs uri$ do
                renderProject mpost proj
                renderAdminBar authenticated uri
