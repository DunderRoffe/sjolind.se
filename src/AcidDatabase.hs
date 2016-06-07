{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
module AcidDatabase where

import Data.Acid
import Data.Acid.Advanced
import Data.Text (Text)
import Data.Serialize.Get (runGet)
import Data.Typeable
import Data.Maybe
import qualified Data.Map.Strict as Map

import Data.SafeCopy
import System.Environment

import Control.Lens (makeLenses)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import AbsDatabase

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''Project)
$(deriveSafeCopy 0 'base ''File)
$(deriveSafeCopy 0 'base ''Post)
$(deriveSafeCopy 0 'base ''Comment)

updateProject :: Project -> Update Database ()
updateProject p@(Project name _ _ _ _) = do
  (Database m) <- get
  put (Database (Map.insert name p m))

getProject :: Text -> Query Database (Maybe Project)
getProject k = do
  (Database m) <- ask
  return (Map.lookup k m)

updatePost :: Text -> Post -> Update Database ()
updatePost projectName post = do
  (Database m) <- get
  let mproj = Map.lookup projectName m
  case mproj of
    Nothing   -> return ()
    Just proj -> do
      let postsMap' = Map.insert (postHeading post) post (projectPostsMap proj)
          proj'     = proj { projectPostsMap = postsMap'}
      put (Database (Map.insert projectName proj' m))

getPost :: Text -> Text -> Query Database (Maybe Post)
getPost projectName postHeading = do
  (Database m) <- ask
  case Map.lookup projectName m of
    Nothing -> return Nothing
    Just project -> return $ Map.lookup postHeading (projectPostsMap project)

$(makeAcidic ''Database ['updateProject, 'getProject, 'updatePost, 'getPost])
