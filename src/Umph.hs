{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import API
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.List (unfoldr)
import Data.Maybe
import Debug.Trace
import Formatting
import Network.HTTP.Conduit
import System.Environment

newtype Link = Link String

data Page = Page { nextPageToken :: Maybe String
                 , entries :: [Link]
                 } deriving (Show)

data UserPage = UserPage { playlistId :: String
                         } deriving (Show)

instance FromJSON Link where
    parseJSON (Object v) = Link . formatToString videoUrl
                           <$> ((v .: "contentDetails") >>= (.: "videoId"))

instance Show Link where
    show (Link s) = s

instance FromJSON Page where
    parseJSON (Object v) = Page <$> (v .:? "nextPageToken") <*> (v .: "items")

instance Monoid Page where
    mempty = Page Nothing []
    mappend (Page _ b) (Page c d) = Page c $ b ++ d

instance FromJSON UserPage where
    parseJSON (Object v) = UserPage <$> (head <$> (v .: "items") >>= (.: "contentDetails") >>= (.: "relatedPlaylists") >>= (.: "uploads"))

playlistUrl   = "https://www.googleapis.com/youtube/v3/playlistItems?key=" % string % "&part=contentDetails&playlistId=" % string % "&maxResults=" % int
uploadUrl     = "https://www.googleapis.com/youtube/v3/channels?key=" % string % "&part=contentDetails&forUsername=" % string
videoUrl      = "https://www.youtube.com/watch?v=" % string
testPlaylist = "PL_XqGBfpM20gxIT0C3gA68eVe-LS7iFrC"

downloadPage url = lift (decode <$> simpleHttp url) >>= MaybeT . return

downloadPlaylist :: String -> Int -> MaybeT IO Page
downloadPlaylist id' maxResults = downloadAllPages $ Just ""
    where url = formatToString playlistUrl youtubeApiKey id' maxResults
          download = downloadPage . (url ++) . maybe "" ("&pageToken=" ++)
          downloadAllPages Nothing = return mempty
          downloadAllPages token   = do
            page <- download token
            (page `mappend`) <$> downloadAllPages (nextPageToken page)

downloadUserUploads :: String -> MaybeT IO Page
downloadUserUploads username = do
  id' <- playlistId <$> downloadPage url
  downloadPlaylist id' 50
    where url = formatToString uploadUrl youtubeApiKey username

printLinks :: Maybe Page -> IO ()
printLinks = maybe (return ()) (mapM_ print . entries)

-- Just need to argparse

main = do
  -- mPage <- runMaybeT $ downloadPlaylist testPlaylist 50
  -- printLinks mPage
  uPage <- runMaybeT $ downloadUserUploads "birgirpall"
  printLinks uPage
