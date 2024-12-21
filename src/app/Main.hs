{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestHeader)

-- import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
  -- maybeValue <- decode <$> LBS.readFile "test.json"
  -- case (maybeValue :: Maybe IPRes) of
  --   Nothing -> putStrLn "parsing failed"
  --   Just value -> print value

  ip_a <- fetchAPI "https://httpbin.org/ip"
  putStrLn $ "your ip:" ++ origin ip_a

  info <- fetchAPI $ "https://ipapi.co/" ++ origin ip_a ++ "/json/"
  putStrLn $ "info:" ++ show (info :: IPInfo)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

newtype IPRes = IPRes {origin :: String} deriving (Show, Generic)

instance FromJSON IPRes

data IPInfo = IPInfo {ip :: String, city :: Maybe String, region :: Maybe String, country :: Maybe String} deriving (Show, Generic)

instance FromJSON IPInfo

fetchAPI :: (FromJSON a) => String -> IO a
fetchAPI url =
  getResponseBody <$> httpJSON infoReq
  where
    infoReq =
      parseRequest_ url
        -: setRequestHeader "User-Agent" ["My Haskell App/1.0"]
