{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveDataTypeable #-}
module AbsDatabase where

import qualified Constants as C
import Data.Typeable
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Map.Strict
import Text.Blaze
import Data.ByteString (ByteString)

data Database = EmptyDatabase | Database (Map Text Project)
newDB :: Database
newDB = Database projMap
  where projMap  = insert "Main" (Project "Main" "## Hello World" postsMap filesMap []) empty
        filesMap = empty
        postsMap = insert "Head" (Post author "Head" "Date" "Text" []) empty
        author   = Author C.authorName "/Main/file/author.jpeg" (toStrict C.serverUri)

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
