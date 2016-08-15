{-# LANGUAGE OverloadedStrings #-}
module Auth where

import Network.HTTP.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Data.Monoid (mconcat)
import Data.Text
import Data.Maybe (fromMaybe)

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (liftM, when, unless)

import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

checkAuth :: Text -> Text -> IO Bool
checkAuth "" _  = return False
checkAuth code uri = do
  manager <- newManager tlsManagerSettings
  initialRequest <- parseRequest (unpack (mconcat ["https://indieauth.com/auth"
                                      , "?code=", code
                                      , "&client_id=", uri
                                      , "&redirect_uri=", uri, indieAuthRoute]))

  let request = initialRequest { method = "POST" }
  response <- httpLbs request manager
  return (statusCode (responseStatus response) == 200)

isAuthenticated :: Text -> S.ActionM Bool
isAuthenticated uri = do
    cookieCode <- liftM (fromMaybe "") $ SC.getCookie $ getCookieName uri
    liftIO $ checkAuth cookieCode uri

getCookieName :: Text -> Text
getCookieName uri = mconcat [uri, "_auth"]

indieAuthRoute :: Text
indieAuthRoute = "/auth"

ifAuthenticated :: Text -> S.ActionM () -> S.ActionM ()
ifAuthenticated uri task = do
    authenticated <- isAuthenticated uri
    if authenticated then task
                     else S.status unauthorized401
