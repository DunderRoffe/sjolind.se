{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AbsDatabase where

import Data.Typeable
import Data.Text (Text)
import Data.Map.Strict
import Text.Blaze

data Database = Database (Map ProjectName Project)
newDB :: Database
newDB = Database (insert "Main" (Project "Main" [] [] [])  empty )

data Project = Project ProjectName [Post] [File] [ProjectName]
  deriving (Typeable)

data Post = Post Heading Date Text [Comment]
  deriving (Typeable)

data Comment = Comment Author Date Text [Comment]
  deriving (Typeable)

data File = File UUID Text Text
  deriving (Typeable)

type ProjectName = Text
type UUID = Text
type Heading = Text
type Author = Text
type Date = Text
