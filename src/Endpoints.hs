{-# LANGUAGE OverloadedStrings #-}
module Endpoints where

import qualified Data.Text.Lazy as LT (fromStrict)

import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC
import Data.Acid
import Page
import AbsDatabase
import AcidDatabase
import Auth
import CommonScotty

import Post

import Endpoints.ProjectEndpoints
import Endpoints.StartupEndpoints

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Network.HTTP.Types

endpoints :: AcidState Database -> S.ScottyM ()
endpoints db = do
    -- Ordering matters
    startupEndpoints db
    projectEndpoints db

    S.get "/auth" $
          flip S.rescue (\_ -> S.status badRequest400) $ do
            code <- S.param "code"
            uri <- getServerUri db
            authenticated <- liftIO $ checkAuth code uri
            when authenticated $
              SC.setSimpleCookie (getCookieName uri) code
            S.redirect $ LT.fromStrict uri
