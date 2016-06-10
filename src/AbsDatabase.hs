{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AbsDatabase where

import Data.Typeable
import Data.Text (Text)
import Data.Map.Strict
import Text.Blaze
import Data.ByteString.Lazy (ByteString)

data Database = Database (Map Text Project)
newDB :: Database
newDB = Database projMap
  where projMap  = insert "Main" (Project "Main" "## Hello World" postsMap filesMap []) empty
        filesMap = insert "test.txt" (File "test.txt" "Detta är en text fil") empty
        postsMap = insert "Head" (Post author "Head" "Date" "Text" []) empty
        author   = Author "MrLel" "/img/coolpicture.jpg" "www.sjolind.se"

data Project = Project {
    projectName        :: Text,
    projectAboutpage   :: Text,
    projectPostsMap    :: (Map Text Post),
    projectFilesMap    :: (Map Text File),
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

data File = File Text ByteString
  deriving (Typeable)

data Author = Author {
    authorName  :: Text,
    authorImage :: Text,
    authorUri   :: Text
  } deriving (Typeable)
