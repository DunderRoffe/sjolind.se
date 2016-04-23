{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE (encodeUtf8)
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC
import Data.Monoid (mconcat, mempty)
import Data.Acid
import Blog
import Page
import Constants

import Data.Maybe (fromMaybe)
import Control.Monad (liftM, when)
import Control.Monad.IO.Class (liftIO)

import Network.Wai.Middleware.Static
import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Text.Blaze
import Text.Blaze.Html.Renderer.Text (renderHtml)

main :: IO()
main = do
  blog <- openLocalStateFrom "db/" (newBlog "My blog")
  secretRough <- liftIO $ readFile "secretFile"
  let cookieName = "sjolind.se"
  S.scotty 3000 $ do
    S.middleware $ staticPolicy (noDots >-> addBase "static")

    S.get "" $ do
          blog' <- liftIO $ query blog GetBlog
          let blogHTML = formatBlogAsHTML blog'
          cookieCode <- liftM (fromMaybe "") $ SC.getCookie cookieName
          let authenticated = cookieCode /= ""
          S.html $ renderHtml $ htmlPage blogHTML authenticated

    S.get "/auth" $
          flip S.rescue (\_ -> S.status badRequest400) $ do
            code <- S.param "code"
            authenticated <- liftIO $ checkAuth $ LT.fromStrict code
            when authenticated $
              SC.setSimpleCookie cookieName code
            S.redirect serverUri 

    S.post "" $ flip S.rescue (\_ -> S.status unauthorized401) $ do
          cookieCode <- liftM (fromMaybe "") $ SC.getCookie cookieName
          authenticated <- liftIO $ checkAuth $ LT.fromStrict cookieCode
          if authenticated
            then do
              contentType <- S.header "Content-Type"
              bp <- case contentType of
                (Just "application/json") -> S.jsonData
                (Just "application/x-www-form-urlencoded") -> do
                  heading <- S.param "heading"
                  date    <- S.param "date"
                  content <- S.param "content"
                  return (BlogPost heading date content)
              liftIO $ update blog (AddBlogPost bp)
              S.redirect serverUri
            else
              S.raise "unathorized"

checkAuth :: LT.Text -> IO Bool
checkAuth ""   = return False
checkAuth code = do
  let clientId    = LT.unpack serverUri
      redirectUri = mconcat [clientId, "/auth"]
  manager <- newManager tlsManagerSettings

  initialRequest <- parseUrl ("https://indieauth.com/auth"
                              ++ "?code=" ++ LT.unpack code
                              ++ "&client_id=" ++ clientId
                              ++ "&redirect_uri=" ++ redirectUri)
  let request = initialRequest { method = "POST" }

  response <- httpLbs request manager
  return (statusCode (responseStatus response) == 200)
