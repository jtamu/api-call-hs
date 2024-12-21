{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api (IPInfo, fetchAPI, origin) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestHeader)

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
