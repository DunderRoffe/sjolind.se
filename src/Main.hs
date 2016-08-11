{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE (encodeUtf8, decodeUtf8)
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

import Post
import Project

import Data.Maybe (fromMaybe)
import Control.Monad (liftM, when, unless)
import Control.Monad.IO.Class (liftIO)

import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Text.Blaze

import qualified Data.Map as Map

cookieName = "sjolind.se"
main :: IO()
main = do
  db <- openLocalStateFrom "db/" emptyDb
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")

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

    S.get "/auth" $
          flip S.rescue (\_ -> S.status badRequest400) $ do
            code <- S.param "code"
            uri <- getServerUri db
            authenticated <- liftIO $ checkAuth code uri
            when authenticated $
              SC.setSimpleCookie cookieName code
            S.redirect $ LT.fromStrict uri

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

getServerUri :: AcidState Database -> S.ActionM T.Text
getServerUri db = do
      (Author _ _ uri) <- liftIO $ query db GetAuthor
      return uri

isAuthenticated :: T.Text -> S.ActionM Bool
isAuthenticated uri = do
  cookieCode <- liftM (fromMaybe "") $ SC.getCookie cookieName
  authenticated <- liftIO $ checkAuth cookieCode uri
  return True -- authenticated

viewProj :: AcidState Database -> T.Text -> T.Text -> S.ActionM ()
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
      cookieCode <- liftM (fromMaybe "") $ SC.getCookie cookieName
      let authenticated = cookieCode /= ""
      vs  <- liftIO $ query db GetVerificationUris
      S.html $ renderCore vs uri$ do
        renderProject mpost proj
        renderAdminBar authenticated uri

checkAuth :: T.Text -> T.Text -> IO Bool
checkAuth "" _  = return False
checkAuth code uri = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseUrl (T.unpack (mconcat ["https://indieauth.com/auth"
                                      , "?code=", code
                                      , "&client_id=", uri
                                      , "&redirect_uri=", uri, "/auth"]))

  let request = initialRequest { method = "POST" }
  response <- httpLbs request manager
  return (statusCode (responseStatus response) == 200)
