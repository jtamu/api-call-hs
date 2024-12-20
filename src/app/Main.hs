{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, Value (Object), decode, parseJSON, (.:))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser)
import Network.HTTP.Simple (getResponseBody, httpLBS, httpLbs, parseRequest_, setRequestHeader)

-- import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = do
  -- maybeValue <- decode <$> LBS.readFile "test.json"
  -- case (maybeValue :: Maybe IPRes) of
  --   Nothing -> putStrLn "parsing failed"
  --   Just value -> print value

  res <- httpLbs $ parseRequest_ "https://httpbin.org/ip"
  let ipMaybe = decode (getResponseBody res)
  case (ipMaybe :: Maybe IPRes) of
    Nothing -> putStrLn "parsing failed"
    Just ip_a -> do
      putStrLn $ "your ip:" ++ origin ip_a

      let infoReq =
            parseRequest_ ("https://ipapi.co/" ++ origin ip_a ++ "/json/")
              -: setRequestHeader "User-Agent" ["My Haskell App/1.0"]

      infoRes <- httpLBS infoReq

      let infoMaybe = decode (getResponseBody infoRes)
      case (infoMaybe :: Maybe IPInfo) of
        Nothing -> putStrLn "parsing failed"
        Just info -> print info

(-:) :: t1 -> (t1 -> t2) -> t2
x -: f = f x

newtype IPRes = IPRes {origin :: String} deriving (Show)

instance FromJSON IPRes where
  parseJSON :: Value -> Parser IPRes
  parseJSON (Object v) = IPRes <$> v .: Key.fromString "origin"
  parseJSON _ = mempty

data IPInfo = IPInfo {ip :: String, city :: Maybe String, region :: Maybe String, country :: Maybe String} deriving (Show)

instance FromJSON IPInfo where
  parseJSON :: Value -> Parser IPInfo
  parseJSON (Object v) =
    IPInfo
      <$> v .: Key.fromString "ip"
      <*> v .: Key.fromString "city"
      <*> v .: Key.fromString "region"
      <*> v .: Key.fromString "country"
  parseJSON _ = mempty
