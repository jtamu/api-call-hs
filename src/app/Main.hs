{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSONEither, parseRequest_, setRequestHeader)

-- import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
  -- maybeValue <- decode <$> LBS.readFile "test.json"
  -- case (maybeValue :: Maybe IPRes) of
  --   Nothing -> putStrLn "parsing failed"
  --   Just value -> print value

  res <- httpJSONEither "https://httpbin.org/ip"
  let ipEither = getResponseBody res
  case ipEither of
    Left msg -> print msg
    Right ip_a -> do
      putStrLn $ "your ip:" ++ origin ip_a

      let infoReq =
            parseRequest_ ("https://ipapi.co/" ++ origin ip_a ++ "/json/")
              -: setRequestHeader "User-Agent" ["My Haskell App/1.0"]

      infoRes <- httpJSONEither infoReq

      let infoEither = getResponseBody infoRes
      case infoEither of
        Left msg -> print msg
        Right info -> print (info :: IPInfo)

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

newtype IPRes = IPRes {origin :: String} deriving (Show, Generic)

instance FromJSON IPRes

data IPInfo = IPInfo {ip :: String, city :: Maybe String, region :: Maybe String, country :: Maybe String} deriving (Show, Generic)

instance FromJSON IPInfo
