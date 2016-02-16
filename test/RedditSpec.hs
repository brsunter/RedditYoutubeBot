{-# LANGUAGE OverloadedStrings #-}
module RedditSpec where

import Test.Hspec
import Reddit
import Data.Text (unpack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Parses URL's Correctly" $ do
    it "Can get video ID from a Post's Long URL " $ do
      let validLongPost = Post "Test" "youtube.com" 123 "http://youtube.com/watch?v=abcd123" 
      (videoIdFromPost validLongPost) `shouldBe` (Just (unpack "abcd123"))

    it "Can get video ID from a Post's Short URL" $ do
      let validShortPost = Post "Test" "youtube.com" 123 "https://youtu.be/abcd123" 
      (videoIdFromPost validShortPost) `shouldBe` (Just (unpack "abcd123"))
