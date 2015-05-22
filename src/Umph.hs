{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Main where

import API (youtubeApiKey)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Maybe
import Formatting
import Network.HTTP.Conduit
import System.Environment

data Arguments = Args { type' :: Char
                      , id' :: String
                      } deriving (Show)

setType (Args _ i) t = Args t i
setId   (Args t _) i = Args t i 

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

downloadPage url = lift (decode <$> simpleHttp url) >>= MaybeT . return

downloadPlaylist :: String -> Int -> MaybeT IO Page
downloadPlaylist id' maxResults = downloadAllPages $ Just ""
    where url = formatToString playlistUrl youtubeApiKey id' maxResults
          download = downloadPage . (url ++) . maybe "" ("&pageToken=" ++)
          downloadAllPages Nothing = return mempty
          downloadAllPages token   = do
            page <- download token
            (page `mappend`) <$> downloadAllPages (nextPageToken page)

downloadUserUploads :: String -> Int -> MaybeT IO Page
downloadUserUploads username maxResults = do
  id' <- playlistId <$> downloadPage url
  downloadPlaylist id' maxResults
    where url = formatToString uploadUrl youtubeApiKey username

printLinks :: Maybe Page -> IO ()
printLinks = maybe (return ()) (mapM_ print . entries)

parseArgs da (a:b:args) 
    | a == "-t" && (b == "p" || b == "u") = parseArgs (setType da $ head b) args
    | a == "-tu" || a == "-tp"            = parseArgs (setType da $ last a) $ b : args
parseArgs da (a:args) = parseArgs (setId da a) args
parseArgs da [] = da

main = do
  args <- parseArgs (Args 'p' "") <$> getArgs
  page <- runMaybeT $ case args of
                        (Args 'p' pId) -> downloadPlaylist pId 50
                        (Args 'u' uId) -> downloadUserUploads uId 50
                        _              -> undefined
  printLinks page
