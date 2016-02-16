{-# LANGUAGE OverloadedStrings #-}
module Reddit
  ( Post(..)
  , videoIdFromPost
  , filterYoutube
  , postStream
  ) where

import           Control.Applicative       (pure, (<$>), (<*>))
import           Control.Concurrent        (threadDelay)
import           Control.Exception         (Exception, try)
import           Control.Lens              ((&), (.~), (^.), (^?))
import           Data.Aeson
import           Data.Foldable             (for_)
import           Data.Text                 (Text, append, pack, unpack)
import           Network.HTTP.Client       (HttpException (StatusCodeException))
import           Network.HTTP.Types.Status (Status (..))
import           Network.URL               (URL (..), URLType (Absolute), host,
                                            importURL, url_params, url_path)
import           Network.Wreq
import qualified Network.Wreq.Session      as S
import           Pipes
import qualified Pipes.Prelude             as P

data Post = Post
  { title  :: !Text
  , domain :: !Text
  , score  :: Int
  , url    :: !Text
  } deriving (Show)

instance FromJSON Post where
  parseJSON (Object v) = do
    objectData <- v .: "data"
    title  <- objectData .: "title"
    domain <- objectData .: "domain"
    score  <- objectData .: "score"
    url    <- objectData .: "url"
    return (Post title domain score url)

data GetPostsResult = GetPostsResult
  { posts :: [Post]
  , after :: Maybe Text
  } deriving (Show)

instance FromJSON GetPostsResult where
  parseJSON (Object v) = do
    rootData   <- v        .:  "data"
    posts      <- rootData .:  "children"
    afterCode  <- rootData .:? "after"
    return (GetPostsResult posts afterCode)

redditVideoUrl = "https://www.reddit.com/r/all/top/.json?sort=top&t=day&after="

retryWithDelay :: IO a -> IO a
retryWithDelay x = threadDelay 10000000 >> x

fetchPageWithRetries :: S.Session -> Text -> Int -> IO (Maybe GetPostsResult)
fetchPageWithRetries sess afterCode retries = do
  result <- try (fetchPage sess afterCode) :: IO (Either HttpException (Maybe GetPostsResult))
  case result of
    Left (StatusCodeException (Status 503 _ ) _ _) ->
      if retries > 0
        then retryWithDelay (fetchPageWithRetries sess afterCode (retries - 1))
        else return Nothing
    Left _  -> return Nothing
    Right r -> return r

fetchPage :: S.Session -> Text -> IO (Maybe GetPostsResult)
fetchPage sess afterCode = do
  response <- S.get sess (redditVideoUrl ++ unpack afterCode)
  return $ decode (response ^. responseBody)

postStream :: S.Session -> Text -> Producer Post IO ()
postStream sess afterCode = do
  posts <- liftIO (fetchPageWithRetries sess afterCode 4)
  for_ posts $ \(GetPostsResult ps afterParam) -> do
    each ps
    for_ afterParam (postStream sess)


postIsValidDomain :: Post -> Bool
postIsValidDomain x = isValidDomain (domain x)
  where
    isValidDomain :: Text -> Bool
    isValidDomain x = x `elem` validDomains

    validDomains = prependHost [youtubeLongURL, youtubeShortURL]

youtubeShortURL = "youtu.be"
youtubeLongURL = "youtube.com"


prependHost :: [Text] -> [Text]
prependHost hs = append <$> ["www.", "", "m."] <*> hs

filterYoutube :: Pipe Post Post IO ()
filterYoutube = P.filter postIsValidDomain

isLongUrl :: URL -> Bool
isLongUrl u = case url_type u of
  Absolute h -> pack (host h) `elem` prependHost [youtubeLongURL]
  _ -> False

videoIdFromPost :: Post -> Maybe String
videoIdFromPost p = videoIdFromURL (unpack (url p))
  where
    videoIdFromURL :: String -> Maybe String
    videoIdFromURL u = case importURL u of
      Just x  -> if isLongUrl x
                   then videoIdFromLongURL  x
                   else videoIdFromShortURL x
      Nothing -> Nothing

    videoIdFromLongURL :: URL -> Maybe String
    videoIdFromLongURL x =
      case lookup "v" (url_params x) of
        Nothing -> Nothing
        Just "" -> Nothing
        Just p  -> Just p

    videoIdFromShortURL :: URL -> Maybe String
    videoIdFromShortURL url =
      case url_path url of
        "" -> Nothing
        s  -> Just s
