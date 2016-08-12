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
$(deriveSafeCopy 0 'base ''Author)

updateProject :: Project -> Update Database ()
updateProject p@(Project name _ _ _ _) = do
  (Database s m a v) <- get
  put (Database s (Map.insert name p m) a v)

isEmpty :: Query Database Bool
isEmpty = do
  (Database _ m _ _) <- ask
  return (Map.null m)

getProject :: Text -> Query Database (Maybe Project)
getProject k = do
  (Database _ m _ _) <- ask
  return (Map.lookup k m)

updatePost :: Text -> Post -> Update Database ()
updatePost projectName post = do
  (Database s m a v) <- get
  let mproj = Map.lookup projectName m
  case mproj of
    Nothing   -> return ()
    Just proj -> do
      let postsMap' = Map.insert (postHeading post) post (projectPostsMap proj)
          proj'     = proj { projectPostsMap = postsMap'}
      put (Database s (Map.insert projectName proj' m) a v)

getPost :: Text -> Text -> Query Database (Maybe Post)
getPost projectName postHeading = do
  (Database _ m _ _) <- ask
  case Map.lookup projectName m of
    Nothing -> return Nothing
    Just project -> return $ Map.lookup postHeading (projectPostsMap project)

updateAuthor :: Author -> Update Database ()
updateAuthor a = do
    (Database s m _ v) <- get
    put (Database s m a v)

getAuthor :: Query Database Author
getAuthor = do
    (Database _ _ a _) <- ask
    return a

updateMainProject :: Text -> Update Database ()
updateMainProject newMain = do
    (Database s m a v) <- get
    case Map.lookup newMain m of
        Nothing -> error $ "Trying to set main project to nonexisting project: " ++ show newMain
        _       -> put (Database newMain m a v)

getMainProject :: Query Database Project
getMainProject = do
    (Database s m _ _) <- ask
    case Map.lookup s m of
        Nothing  -> error $ "Could not find MainProject: " ++ show s
        (Just p) -> return p

updateVerificationUris :: [Verification] -> Update Database ()
updateVerificationUris v = do
    (Database s m a _) <- get
    put (Database s m a v)

getVerificationUris :: Query Database [Verification]
getVerificationUris = do
    (Database _ _ _ v) <- ask
    return v


$(makeAcidic ''Database ['updateProject, 'getProject,
                         'updatePost, 'getPost,
                         'updateAuthor, 'getAuthor,
                         'updateMainProject, 'getMainProject,
                         'updateVerificationUris, 'getVerificationUris,
                         'isEmpty])
