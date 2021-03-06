{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AbsDatabase where

import Data.Typeable
import Data.Text (Text)
import Data.Map.Strict
import Data.ByteString (ByteString)

type Verification = Text

data Database = Database Text (Map Text Project) Author [Verification]
emptyDb :: Database
emptyDb = Database "" empty (Author "" "" "") []

data Project = Project {
    projectName        :: Text,
    projectAboutpage   :: Text,
    projectPostsMap    :: Map Text Post,
    projectFilesMap    :: Map ByteString File,
    projectSubProjects :: [Text]
  } deriving (Typeable)

data Post = Post {
    postAuthor   :: Author,
    postHeading  :: Text,
    postDate     :: Text,
    postContent  :: Text,
    postComments :: [Comment]
  } deriving (Typeable)

data Comment = Comment {
    commentAuthor   :: Author,
    commentDate     :: Text,
    commentContent  :: Text,
    commentComments :: [Comment]
  } deriving (Typeable)

data File = File ByteString ByteString
  deriving (Typeable)

data Author = Author {
    authorName  :: Text,
    authorImage :: Text,
    authorUri   :: Text
  } deriving (Typeable)
