{-# LANGUAGE OverloadedStrings #-}
module Endpoints.AuthEndpoints where

import qualified Data.Text.Lazy as LT (fromStrict)
import Data.Text

import Auth

import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import Network.HTTP.Types
import qualified Web.Scotty as S
import qualified Web.Scotty.Cookie as SC

authenticate :: Text -> S.ActionM ()
authenticate uri =
          flip S.rescue (\_ -> S.status badRequest400) $ do
            code <- S.param "code"
            authenticated <- liftIO $ checkAuth code uri
            when authenticated $
              SC.setSimpleCookie (getCookieName uri) code
            S.redirect $ LT.fromStrict uri
