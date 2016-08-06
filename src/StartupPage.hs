{-# LANGUAGE OverloadedStrings #-}
module StartupPage where

import Prelude hiding (head, div, id)
import Data.Monoid (mconcat)
import Control.Monad (when)

import Data.Text.Lazy (toStrict)

import Constants

import Text.Blaze
import Text.Blaze.Internal as B
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)

renderStartupPage :: Html
renderStartupPage =
          docTypeHtml $ do
            head $ do
              title $ text $ toStrict serverUri
            body $ do
              form ! action (B.lazyTextValue serverUri) ! method "post" $ do
                authorForm
                mainpageForm
                button ! type_ "submit" $ text "Create"

authorForm :: Html
authorForm = do
            label ! for "author_name" $ text "Author name:"
            input ! id "author_name" ! type_ "text"
                  ! name "author_name" ! placeholder "Mr Bigglesworth"
            input ! name "author_image" ! type_ "file"
            input ! type_ "hidden" ! name "author_uri"
                  ! value (textValue (toStrict serverUri))

mainpageForm :: Html
mainpageForm = do
            label ! for "proj_name" $ text "Project name"
            input ! id "proj_name" ! type_ "text"
                  ! name "proj_name" ! placeholder "My epic blag!"
            label ! for "proj_about" $ text "Project about"
            textarea ! id "proj_about" ! name "proj_about"
                     ! placeholder "Markdown supported" $ return ()
