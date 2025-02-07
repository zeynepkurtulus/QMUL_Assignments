module Fetch (fetchStopPoints) where

import Network.HTTP.Simple (httpLBS, parseRequest, getResponseBody, getResponseStatusCode, Request, Response)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception (try, SomeException)

-- | Fetches stop points from the given URL.
-- Returns either an error message (Left String) or the response body (Right ByteString).
--
-- @param url The URL to fetch stop points from
-- @returns Either an error message or the response body
fetchStopPoints :: String -> IO (Either String L8.ByteString)
fetchStopPoints url = do
    requestResult <- try (parseRequest url) :: IO (Either SomeException Request)
    case requestResult of
        Left ex -> return $ Left $ "Invalid URL: " ++ show ex
        Right request -> do
            responseResult <- try (httpLBS request) :: IO (Either SomeException (Response L8.ByteString))
            case responseResult of
                Left ex -> return $ Left $ "HTTP request failed: " ++ show ex
                Right response -> do
                    let statusCode = getResponseStatusCode response
                    if statusCode == 200
                        then return $ Right $ getResponseBody response
                        else return $ Left $ "HTTP error: " ++ show statusCode