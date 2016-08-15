{-# LANGUAGE OverloadedStrings #-}
module Endpoints.AuthEndpoints where

import qualified Data.Text.Lazy as LT (fromStrict)
import Data.Acid
import Data.Text

import AbsDatabase
import AcidDatabase
import Auth
import CommonScotty

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Network.HTTP.Types
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

authEndpoints :: AcidState Database -> S.ScottyM ()
authEndpoints db =
    S.get (S.literal (unpack indieAuthRoute)) $
          flip S.rescue (\_ -> S.status badRequest400) $ do
            code <- S.param "code"
            uri <- getServerUri db
            authenticated <- liftIO $ checkAuth code uri
            when authenticated $
              SC.setSimpleCookie (getCookieName uri) code
            S.redirect $ LT.fromStrict uri
