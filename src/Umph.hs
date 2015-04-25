{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Network.HTTP.Conduit

testUrl = "https://gdata.youtube.com/feeds/api/playlists/PL_XqGBfpM20gTwBHV-yCoKwJpayIy-jzL?alt=json&start-index=1&max-results=50&v=2"

data Page = Page { totalEntries :: Integer
                 , entries :: [Link]
                 } deriving (Show)

newtype Link = Link String

instance FromJSON Page where
    parseJSON (Object v) = v .: "feed" >>= 
                           \x -> Page <$> (x .: "openSearch$totalResults" >>= (.: "$t"))
                                 <*> (x .: "entry")

instance FromJSON Link where
    parseJSON (Object v) = Link <$> ((head <$> v .: "link") >>= (.: "href"))

instance Show Link where
    show (Link s) = s

downloadPage :: String -> MaybeT IO Page
downloadPage url = lift (decode <$> simpleHttp url) >>= MaybeT . return

printLinks :: Page -> IO ()
printLinks = mapM_ print . entries

main = do
  runMaybeT $ downloadPage testUrl >>= lift . printLinks
  return ()
