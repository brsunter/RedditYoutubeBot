{-# LANGUAGE OverloadedStrings #-}
module Scrape (createPlaylistFromReddit) where
import qualified Data.Text            as T
import           Network.Wreq.Session as S (withSession)
import           Pipes                (runEffect, (>->))
import           Pipes.Prelude        as P (take)
import           Reddit
import           Utility
import           YouTube

createPlaylistFromReddit :: IO ()
createPlaylistFromReddit = do
  today       <- formattedTodayDate
  credentials <- readCredentialsFromEnv
  accessToken <- getYoutubeAccessToken credentials
  case accessToken of
    Nothing    -> error "Cannot obtain access token"
    Just token -> do
      pl <- makeYoutubePlaylist token (T.concat [videoMessage, today])
      case pl of
        Nothing       -> error "Cannot Make Playlist"
        Just playlist -> do
          print pl
          S.withSession $ \sess -> runEffect (postStream sess "" >-> filterYoutube >-> P.take 40 >-> postToYoutube sess token playlist)

