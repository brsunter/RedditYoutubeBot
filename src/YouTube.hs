{-# LANGUAGE OverloadedStrings #-}
module YouTube
  ( postToYoutube
  , makeYoutubePlaylist
  , getYoutubeAccessToken
  , readCredentialsFromEnv
  ) where

import           Control.Applicative       (pure, (<$>), (<*>))
import           Control.Exception         (try)
import           Control.Lens              ((&), (.~), (^.), (^?))
import           Data.Aeson
import           Data.Aeson.Lens           (key, _String)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BSC
import qualified Data.ByteString.Lazy      as BL
import           Data.Text                 (Text, append, concat, pack, unpack)
import           Network.HTTP.Client       (HttpException (StatusCodeException))
import           Network.HTTP.Types.Status (Status (..))
import           Network.URL               (URL (..), URLType (Absolute), host,
                                            importURL, url_params, url_path)
import           Network.Wreq
import qualified Network.Wreq.Session      as S
import           Network.Wreq.Types        (Postable, postPayload)
import           Pipes
import           Prelude                   hiding (concat)
import           Reddit
import           System.Environment        (lookupEnv)

youtubeTokenURL :: AccessTokenCredentials -> Text
youtubeTokenURL creds =
  concat [ "https://www.googleapis.com/oauth2/v3/token?client_secret="
         , clientSecret creds
         , "&grant_type=refresh_token&refresh_token="
         , clientRefreshToken creds
         , "&client_id="
         , clientId creds
         ]

data AccessTokenCredentials = AccessTokenCredentials
  { clientId           :: Text
  , clientSecret       :: Text
  , clientRefreshToken :: Text
  }

googleBaseURL     = "https://www.googleapis.com/youtube/v3" :: String
googlePlaylistAPI = googleBaseURL ++ "/playlists?part=snippet,status"
googleItemAPI     = googleBaseURL ++ "/playlistItems?part=snippet"
refreshTokenFilename = "refreshtoken"

getYoutubeAccessToken :: AccessTokenCredentials -> IO (Maybe Text)
getYoutubeAccessToken creds = do
  r <- post (unpack $ youtubeTokenURL creds) emptyPostParam
  return (r ^? responseBody . key "access_token" . _String)

  where
    emptyPostParam :: FormParam
    emptyPostParam = x := x
      where
        x  = "" :: BS.ByteString

googleAuthOptions :: Text -> Options
googleAuthOptions token = defaults
  & header "Authorization" .~ [BS.concat ["Bearer ", BSC.pack $ unpack token]]
  & header "Content-Type"  .~ ["application/json"]

makeYoutubePlaylist ::  Text -> Text -> IO (Maybe Text)
makeYoutubePlaylist token name = extractIdResponse <$> postWith (googleAuthOptions token) googlePlaylistAPI (PlaylistBody name)

extractIdResponse :: Response BL.ByteString -> Maybe Text
extractIdResponse x =  x ^? responseBody . key "id" . _String

data PlaylistBody = PlaylistBody { name :: Text }

instance ToJSON PlaylistBody where
  toJSON (PlaylistBody name) = object [ "snippet" .= (object ["title" .= name])
                                      , "status"  .= (object ["privacyStatus" .= ("public" :: Text)])]

instance Postable PlaylistBody where
  postPayload a = postPayload (toJSON a)

data PlaylistItemBody = PlaylistItemBody { playlistId :: Text, videoId :: Text }

instance ToJSON PlaylistItemBody where
  toJSON (PlaylistItemBody playlistId videoId) =
    object [ "snippet"    .= (object [ "playlistId" .= playlistId
                                     , "resourceId" .= (object [ "videoId" .= videoId
                                                               , "kind"    .= ("youtube#video" :: Text)])])]
instance Postable PlaylistItemBody where
  postPayload a = postPayload (toJSON a)

postToYoutube :: S.Session -> Text -> Text -> Consumer Post IO ()
postToYoutube sess token playlistId = do
  post <- await
  liftIO (case videoIdFromPost post of
    Nothing      -> return ()
    Just videoId -> do
      _ <- addVideoToPlaylistIgnoring404 sess token playlistId (pack videoId)
      print post)
  postToYoutube sess token playlistId

  where
    addVideoToPlaylist :: S.Session -> Text -> Text -> Text -> IO (Maybe Text)
    addVideoToPlaylist sess token playlist video =
      extractIdResponse <$> S.postWith (googleAuthOptions token) sess googleItemAPI (PlaylistItemBody playlist video)

    addVideoToPlaylistIgnoring404 :: S.Session -> Text-> Text -> Text -> IO (Maybe Text)
    addVideoToPlaylistIgnoring404 sess token playlist video  = do
      result <- try (addVideoToPlaylist sess token playlist video) :: IO (Either HttpException (Maybe Text))
      case result of
        Left (StatusCodeException (Status 404 _ ) _ _) -> return Nothing
        Right r -> return r

readCredentialsFromEnv :: IO AccessTokenCredentials
readCredentialsFromEnv = do
  refreshEnv <- readEnvVariable clientRefreshTokenEnv
  secretEnv  <- readEnvVariable clientSecretEnv
  idEnv      <- readEnvVariable clientIdEnv
  let envCreds = AccessTokenCredentials <$> idEnv <*> secretEnv <*> refreshEnv
  case envCreds of
    Just c -> return c
    Nothing -> error "Could not read Client ID, Client Secret, and Refresh Token from ENV vars"

  where
    readEnvVariable :: String -> IO (Maybe Text)
    readEnvVariable envVar = (pack <$>) <$> lookupEnv envVar

    clientRefreshTokenEnv = "YOUTUBE_REFRESH_TOKEN" :: String
    clientIdEnv           = "YOUTUBE_CLIENT_ID"     :: String
    clientSecretEnv       = "YOUTUBE_CLIENT_SECRET" :: String
