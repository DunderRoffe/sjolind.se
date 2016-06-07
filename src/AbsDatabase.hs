{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AbsDatabase where

import Data.Typeable
import Data.Text (Text)
import Data.Map.Strict
import Text.Blaze

data Database = Database (Map Text Project)
newDB :: Database
newDB = Database projMap
  where projMap  = insert "Main" (Project "Main" "## Hello World" postsMap empty []) empty
        postsMap = insert "Head" (Post "Head" "Date" "Text" []) empty

data Project = Project {
    projectName        :: Text,
    projectAboutpage   :: Text,
    projectPostsMap    :: (Map Text Post),
    projectFilesMap    :: (Map Text File),
    projectSubProjects :: [Text]
  } deriving (Typeable)

data Post = Post {
    postHeading  :: Text,
    postDate     :: Text,
    postContent  :: Text,
    postComments :: [Comment]
  } deriving (Typeable)

data Comment = Comment {
    commentAuthor   :: Text,
    commentDate     :: Text,
    commentContent  :: Text,
    commentComments :: [Comment]
  } deriving (Typeable)

data File = File Text Text
  deriving (Typeable)
