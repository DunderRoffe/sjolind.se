{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
module AcidDatabase where

import Data.Acid
import Data.Acid.Advanced

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
updateProject p@(Project name _ _ _) = do
  (Database m) <- get
  put (Database (Map.insert name p m))

getProject :: ProjectName -> Query Database (Maybe Project)
getProject k = do
  (Database m) <- ask
  return (Map.lookup k m)

$(makeAcidic ''Database ['updateProject, 'getProject])
