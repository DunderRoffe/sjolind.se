{-# LANGUAGE OverloadedStrings #-}
module Endpoints where

import qualified Web.Scotty as S
import Data.Acid
import AbsDatabase
import AcidDatabase

import Endpoints.ProjectEndpoints
import Endpoints.AuthEndpoints
import Endpoints.StartupEndpoints

endpoints :: AcidState Database -> S.ScottyM ()
endpoints db = do
    -- Ordering matters
    startupEndpoints db
    authEndpoints db
    projectEndpoints db
