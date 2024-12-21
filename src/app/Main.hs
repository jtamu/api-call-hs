module Main where

import Api (IPInfo, fetchAPI, origin)

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
