{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
module Blog where

import Data.Text (Text)
import Data.Monoid (mconcat)

import Data.Acid
import Data.Acid.Advanced

import Data.Serialize.Get (runGet)
import Data.Typeable
import Data.Aeson

import Data.SafeCopy
import System.Environment

import Control.Lens (makeLenses)
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State

import Prelude hiding (div)
import Text.Blaze
import Text.Blaze.Html5 hiding (base, map)
import Text.Blaze.Html5.Attributes

type Heading = Text
type Content = Text
type Date    = Text

data BlogPost = BlogPost Heading Date Content
  deriving (Show, Ord, Eq, Read, Typeable)

instance FromJSON BlogPost where
  parseJSON (Object v) = BlogPost <$> v .: "heading"
                                  <*> v .: "date"
                                  <*> v .: "content"
  parseJSON _ = mzero
$(deriveSafeCopy 0 'base ''BlogPost)

data Blog = Blog Heading [BlogPost]
  deriving (Show, Ord, Eq, Read, Typeable)

$(deriveSafeCopy 0 'base ''Blog)

newBlog :: Text -> Blog
newBlog h = Blog h []

addBlogPost :: BlogPost -> Update Blog ()
addBlogPost bp = do
  (Blog h bps) <- get
  put (Blog h (bp:bps))

getBlog :: Query Blog Blog
getBlog = ask

formatBlogAsHTML :: Blog -> Markup
formatBlogAsHTML (Blog h bps) =
  div ! class_ "blog blogContainer" $ do
    div ! class_ "blog blogHeading" $ text h
    div ! class_ "blog blogPosts" $ mconcat (map formatBlogPostAsHTML bps)

formatBlogPostAsHTML :: BlogPost -> Markup
formatBlogPostAsHTML (BlogPost h d c) =
  div ! class_ "blogPost blogPostContainer" $ do
    div ! class_ "blogPost blogPostHeading" $ text (mconcat [h, " ", d])
    div ! class_ "blogPost blogPostContent" $ text c

$(makeAcidic ''Blog ['addBlogPost, 'getBlog])
