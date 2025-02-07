{-|
Server module implements the server that processes requests from clients and generates responses.
-}
module Server where

import Database (saveResponse)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar, modifyMVar_)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, LocalTime)
import Types (Request(..), Response(..), RequestQueue)


-- | Converts UTC time to local time
getLocalTime :: IO LocalTime
getLocalTime = do
    utcTime <- getCurrentTime -- Fetch the current UTC time
    timeZone <- getCurrentTimeZone -- Fetch the current UTC time
    return $ utcToLocalTime timeZone utcTime -- Convert UTC time to local time using the timezone

-- | The main server loop that continuously processes requests from the queue.
-- It retrieves requests from the queue, generates responses, saves them to the database and recursively continues processing until all requests are handled.
serverLoop :: RequestQueue -> FilePath -> MVar () -> IO ()
serverLoop queue dbPath dbLock = do
    putStrLn "Server: Waiting for requests..." -- Notify that the server is waiting for requests for debugging 

    requests <- takeMVar queue -- Atomically take the current state of the queue
    case requests of
        [] -> do
            putStrLn "Server: Queue is empty, waiting for requests." -- Queue is empty 
            putMVar queue [] -- Put the empty queue back
            threadDelay 100000 -- Wait for 0.1 seconds before retrying
            serverLoop queue dbPath dbLock -- Recursively call the server loop
        (req : rest) -> do
            currentTime <- getCurrentTime -- Get the current UTC time
            let response = Response -- Generate a response object
                  { responseId = requestId req
                  , responseTime = currentTime
                  , responseContent = "Response to: " ++ requestContent req
                  }

            putStrLn $ "Server processed request " ++ show (requestId req)

            modifyMVar_ dbLock $ \_ -> do -- Use the database lock to ensure thread-safe access
                saveResponse dbPath response -- Save the response to the database
                return ()

            putMVar queue rest  -- Put the remaining requests back into the queue
            serverLoop queue dbPath dbLock  -- Recursively call the server loop to process the next request


-- | Function to format a single request-response pair.
-- This function prepares request-response data for logging by creating a well-structured string.
formatLog :: Request -> Response -> String
formatLog req res = unlines
    [ "  {"
    , "    \"RequestID\": " ++ show (requestId req) ++ ","
    , "    \"RequestTime\": \"" ++ show (requestTime req) ++ "\","
    , "    \"ResponseTime\": \"" ++ show (responseTime res) ++ "\","
    , "    \"RequestContent\": \"" ++ requestContent req ++ "\","
    , "    \"ResponseContent\": \"" ++ responseContent res ++ "\""
    , "  }"
    ]

-- | Writes the log as a formatted JSON array to the specified file.
-- The function saves a list of request-response pairs to a file, ensuring a proper JSON format.
writeLog :: FilePath -> [(Request, Response)] -> IO ()
writeLog path logEntries = do
    let formattedLogs = map (uncurry formatLog) logEntries -- Format each request-response pair
    let jsonContent = "[" ++ unlines (zipWith appendComma formattedLogs [1..]) ++ "]"  -- Create a JSON array
    writeFile path jsonContent  -- Write the formatted JSON to the specified file
    putStrLn $ "Logs saved to " ++ path -- Print a success message
  where
    -- Helper function to append a comma unless it's the last entry
    appendComma :: String -> Int -> String
    appendComma entry index
      | index == length logEntries = "  " ++ entry -- If it's the last entry, don't add a comma
      | otherwise = "  " ++ entry ++ ","  -- Otherwise, append a comma
