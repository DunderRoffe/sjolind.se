{-# LANGUAGE OverloadedStrings #-}
module Page where

import Prelude hiding (head, div, id)
import Data.Monoid (mconcat)
import Control.Monad (when)

import Constants

import Text.Blaze
import Text.Blaze.Internal as B
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (title, form, label)

htmlPage :: Html -> Bool -> Html
htmlPage blog authenticated =
          docTypeHtml $ do
            head $ do
              title "sjolind.se"
              link ! rel "stylesheet" ! type_ "text/css" ! href "css/blog.css"
            body $ do
              div ! class_ "about" $ "My name is Viktor Sjölind"
              ul $ do
                li $ a ! href "https://github.com/dunderroffe" ! rel "me" $ "GitHub"
                li $ a ! href "https://bitbucket.org/roffe" ! rel "me" $ "BitBucket"
              blog
              if authenticated
                 then newBlogPost
                 else signInForm

newBlogPost :: Html
newBlogPost =
          form ! action (B.lazyTextValue serverUri) ! method "post" $ do
            div $ do
              label ! for "heading" $ text "Heading"
              input ! id "heading " ! name "heading" ! value ""
                    ! placeholder "Awesome thing!"
            div $ do
              label ! for "date" $ text "Date"
              input ! id "date" ! name "date" ! value "" ! type_ "date"
                    ! placeholder "Date"
            div $ do
              label ! for "content" $ text "Content"
              textarea ! id "content" ! name "content"
                    ! placeholder "BlogPost text..." $ return ()

            div $ do
              input ! type_ "submit"

signInForm :: Html
signInForm =
          form ! action "https://indieauth.com/auth" ! method "get" $ do
            label ! for "indie_auth_url" $ text "Web Address:"
            input ! id "indie_auth_url" ! type_ "text"
                  ! name "me" ! placeholder "yourdomain.com"
            button ! type_ "submit" $ text "Sign In"
            input ! type_ "hidden" ! name "client_id"
                  ! value (B.lazyTextValue serverUri)
            input ! type_ "hidden" ! name "redirect_uri"
                  ! value (B.lazyTextValue (mconcat [serverUri, "/auth"]))
