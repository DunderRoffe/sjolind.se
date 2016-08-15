{-# LANGUAGE OverloadedStrings #-}
module Endpoints.StartupEndpoints where

import qualified Data.Text.Encoding as TE (decodeUtf8)
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LBS (fromStrict, toStrict)
import qualified Data.Text.Lazy as LT (fromStrict)
import Data.Acid

import AbsDatabase
import AcidDatabase
import CommonScotty

import Pages.Core
import Pages.StartupPage

import qualified Web.Scotty as S

import Network.Wai.Parse
import Control.Monad.IO.Class (liftIO)

presentStartupPage :: S.ActionM ()
presentStartupPage = S.html $ renderCore [] "" renderStartupPage

initDB :: AcidState Database -> S.ActionM ()
initDB db = do
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
