{-# LANGUAGE OverloadedStrings #-}
module Webmentions where

import AbsDatabase
import AcidDatabase
import Endpoints.Common

import Data.Acid (AcidState)
import Data.Text
import Data.ByteString.Lazy (ByteString)
import Data.Maybe

import Control.Concurrent (forkIO)

import Network.HTTP.Client hiding (parseUrl)
import Network.HTTP.Client.TLS

type Webmention = (Text, Text)

data Mention = Like Author | Comment Text | Reference Text

processWebmentionAsync :: Webmention -> AcidState Database -> IO ()
processWebmentionAsync w db = do
    forkIO (processWebmention w db)
    return ()

processWebmention :: Webmention -> AcidState Database -> IO ()
processWebmention (source, target) db = do
    host <- getServerUri db 
    case stripPrefix host target of
         Nothing  -> return ()
         Just url -> do
             sourceContent <- getSource source
             case parseSource sourceContent of
                 Nothing -> return ()
                 Just content -> handleMention (parseUrl $ splitOn "/" url) content db

handleMention :: (Text, Text) -> Mention -> AcidState Database -> IO ()
handleMention (project, post) mentionContent db = do
    case post of
        "" -> return () -- Project frontpage mentioned
        _  -> return () -- Project post mentioned

parseUrl :: [Text] -> (Text, Text)
parseUrl [] = ("", "")
parseUrl (project:[])         = (project, "")
parseUrl (project:("post":_)) = (project, "")
parseUrl (project:("file":_)) = (project, "")
parseUrl (project:(post:_))   = (project, post)

parseSource :: ByteString -> Maybe Mention
parseSource content = Nothing -- TODO make actual check of source

getSource :: Text -> IO ByteString
getSource source = do
    manager <- newManager tlsManagerSettings
    initialRequest <- parseRequest $ unpack source

    let request = initialRequest { method = "GET" }
    response <- httpLbs request manager
    return $ responseBody response
