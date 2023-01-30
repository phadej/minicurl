{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString    as BS
import           Test.HUnit         ((@?=))

import MiniCurl

main :: IO ()
main = withLibcurl $ do
    withCurl $ \curl -> do
        curlResponseCode curl >>= print

        bs1 <- curlPerform curl "http://oleg.fi/ClojuTre19.pdf" 188177
        curlResponseCode curl >>= print
        SHA256.hash bs1 @?= "\180A\131A\136<v22\FS\170'\255\166xI\v\217\136\DC3\f@\184\ENQe\187\128\DC3\148\148#/"

        bs2 <- curlPerform curl "http://oleg.fi/ClojuTre19.pdf" 188177
        curlResponseCode curl >>= print
        SHA256.hash bs2 @?= "\180A\131A\136<v22\FS\170'\255\166xI\v\217\136\DC3\f@\184\ENQe\187\128\DC3\148\148#/"
