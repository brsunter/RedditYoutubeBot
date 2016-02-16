{-# LANGUAGE OverloadedStrings #-}
module Utility
  ( videoMessage
  , formattedTodayDate
  ) where

import           Data.Text        (Text (..), pack)
import           Data.Time        (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)

videoMessage = "Top Reddit Videos of " :: Text

formattedTodayDate :: IO Text
formattedTodayDate = (pack . formatTime defaultTimeLocale "%A, %B %e %Y") <$> getCurrentTime
