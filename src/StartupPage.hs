{-# LANGUAGE OverloadedStrings #-}
module StartupPage where

import Prelude hiding (head, div, id)
import Data.Monoid (mconcat)
import Control.Monad (when)

import Data.Text.Lazy (toStrict)

import Text.Blaze
import Text.Blaze.Internal as B
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)

renderStartupPage :: Html
renderStartupPage =
              form ! id "starupForm"
                   ! action ""
                   ! method "post"
                   ! enctype "multipart/form-data" $
                   do
                     div authorForm
                     div mainpageForm
                     button ! type_ "submit" $ text "Create"

authorForm :: Html
authorForm = do
            label ! for "author_name" $ text "Author name:"
            input ! id "author_name" ! type_ "text"
                  ! name "author_name" ! placeholder "Mr Bigglesworth"
            br
            label ! for "author_image" $ text "Picture"
            input ! id "author_image" ! name "author_image" ! type_ "file"
            br
            label ! for "author_verify" $ text "Verification host"
            input ! id "author_verify" ! name "author_verify"
                  ! type_ "text" ! placeholder "https://github.com"
            br
            label ! for "author_uri" $ text "Uri"
            input ! name "author_uri"
                  ! placeholder (textValue "mrbigglesworths.blag")

mainpageForm :: Html
mainpageForm = do
            label ! for "proj_name" $ text "Project name"
            input ! id "proj_name" ! type_ "text"
                  ! name "proj_name" ! placeholder "My epic blag!"
            br
            label ! for "proj_about" $ text "Project about"
            br
            textarea ! id "proj_about" ! name "proj_about"
                     ! placeholder "Markdown supported" $ return ()
