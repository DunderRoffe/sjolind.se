module Endpoints.Common where

import Data.Text
import Data.Acid
import AbsDatabase
import AcidDatabase

import Control.Monad.IO.Class (liftIO, MonadIO)

getServerUri :: MonadIO m => AcidState Database -> m Text
getServerUri db = do
    (Author _ _ uri) <- liftIO $ query db GetAuthor
    return uri
