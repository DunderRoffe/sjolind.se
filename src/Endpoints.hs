{-# LANGUAGE OverloadedStrings #-}
module Endpoints where

import Data.Text
import qualified Data.Text.Lazy as LT (fromStrict)
import qualified Data.Text.Encoding as TE (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Lazy as LBS (fromStrict, toStrict)

import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC
import Data.Monoid (mconcat, mempty)
import Data.Acid
import Page
import StartupPage
import AbsDatabase
import AcidDatabase
import Auth
import CommonScotty

import Post

import Endpoints.ProjectEndpoints

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (liftM, when, unless)

import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Network.HTTP.Types

import Text.Blaze

import qualified Data.Map as Map

endpoints :: AcidState Database -> S.ScottyM ()
endpoints db = do

    -- Startup Page
    S.get "" $ do
          e <- liftIO $ query db IsEmpty
          unless e S.next
          vs <- liftIO $ query db GetVerificationUris
          (Author _ _ uri) <- liftIO $ query db GetAuthor
          S.html $ renderCore vs uri renderStartupPage

    S.post "" $ do
          e <- liftIO $ query db IsEmpty
          unless e S.next
          projectName     <- S.param "proj_name"
          projectAbout    <- S.param "proj_about"

          authorName      <- S.param "author_name"
          authorUri       <- S.param "author_uri"

          verificationUri <- S.param "author_verify"

          ((_, fileinfo): _) <- S.files

          let filesMap  = Map.insert (fileName fileinfo) file Map.empty
              file      = File (fileName fileinfo) (LBS.toStrict (fileContent fileinfo))
              project   = Project projectName projectAbout Map.empty filesMap []
              imgPath   = mconcat [projectName, "/file/", TE.decodeUtf8 (fileName fileinfo)]
              author    = Author authorName imgPath authorUri

          liftIO $ update db (UpdateProject project)
          liftIO $ update db (UpdateAuthor author)
          liftIO $ update db (UpdateMainProject projectName)
          liftIO $ update db (UpdateVerificationUris [verificationUri])
          uri <- getServerUri db
          S.redirect $ LT.fromStrict uri

    -- Normal run
    projectEndpoints db

    S.get "/auth" $
          flip S.rescue (\_ -> S.status badRequest400) $ do
            code <- S.param "code"
            uri <- getServerUri db
            authenticated <- liftIO $ checkAuth code uri
            when authenticated $
              SC.setSimpleCookie (getCookieName uri) code
            S.redirect $ LT.fromStrict uri
