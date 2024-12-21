{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple (JSONException, getResponseBody, httpJSONEither, parseRequest_, setRequestHeader)

-- import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
  -- maybeValue <- decode <$> LBS.readFile "test.json"
  -- case (maybeValue :: Maybe IPRes) of
  --   Nothing -> putStrLn "parsing failed"
  --   Just value -> print value

  res <- httpJSONEither "https://httpbin.org/ip"
  let ipEither = getResponseBody res
  ip_a <- either throwIO pure ipEither
  putStrLn $ "your ip:" ++ origin ip_a

  infoEither <- fetchIPInfo $ origin ip_a
  info <- either throwIO pure infoEither
  putStrLn $ "info:" ++ show info

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

newtype IPRes = IPRes {origin :: String} deriving (Show, Generic)

instance FromJSON IPRes

data IPInfo = IPInfo {ip :: String, city :: Maybe String, region :: Maybe String, country :: Maybe String} deriving (Show, Generic)

instance FromJSON IPInfo

fetchIPInfo :: String -> IO (Either JSONException IPInfo)
fetchIPInfo ip_a = do
  let infoReq =
        parseRequest_ ("https://ipapi.co/" ++ ip_a ++ "/json/")
          -: setRequestHeader "User-Agent" ["My Haskell App/1.0"]

  infoRes <- httpJSONEither infoReq

  return (getResponseBody infoRes)
