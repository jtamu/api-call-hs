{-# LANGUAGE InstanceSigs #-}

module Main where

import Data.Aeson (FromJSON, Value (Object), decode, parseJSON, (.:))
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types (Parser)
import Network.HTTP.Simple (getResponseBody, httpLbs, parseRequest_)

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
    Just ip -> putStrLn $ "your ip:" ++ origin ip

newtype IPRes = IPRes {origin :: String} deriving (Show)

instance FromJSON IPRes where
  parseJSON :: Value -> Parser IPRes
  parseJSON (Object v) = IPRes <$> v .: Key.fromString "origin"
  parseJSON _ = mempty
