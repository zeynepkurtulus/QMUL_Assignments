{-|
Client module implements client behavior for sending requests to the server.
-}
module Client where
import Control.Exception (try, SomeException)
import Database (saveRequest)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, readMVar, modifyMVar)
import System.Random (randomRIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime, LocalTime)
import Types (Request(..), RequestQueue)


-- | Converts UTC time to local time
getLocalTime :: IO LocalTime
getLocalTime = do
    utcTime <- getCurrentTime
    timeZone <- getCurrentTimeZone
    return $ utcToLocalTime timeZone utcTime

-- | Sends a request to the shared request queue and stores it in the database.
sendRequest :: MVar Int -> RequestQueue -> Int -> FilePath -> MVar () -> IO Request
sendRequest counter queue clientId dbPath dbLock = do
    requestId <- modifyMVar counter $ \x -> return (x + 1, x + 1) -- Increment the request counter atomically and assign the new value as the request ID
    currentTime <- getCurrentTime -- Get the current time for timestamping the request
    -- Create a new Request object with the generated ID, timestamp, and client-specific content
    let request = Request
            { requestId = requestId
            , requestTime = currentTime
            , requestContent = "Client " ++ show clientId ++ " Request"
            }

    -- Append the request to the shared request queue in a thread-safe manner
    modifyMVar_ queue $ \requests -> return (requests ++ [request])
    -- Use the database lock to safely save the request to the database
    modifyMVar_ dbLock $ \_ -> do
        saveRequest dbPath request -- Save the request in the SQLite database
        return ()

    putStrLn $ "Client " ++ show clientId ++ " sent request " ++ show requestId
     -- Return the generated request
    return request



-- | Adds a request to the request queue in a thread-safe manner.
enqueueRequest :: RequestQueue -> Request -> IO ()
enqueueRequest queue request = modifyMVar_ queue $ \requests -> do  -- Use `modifyMVar_` to safely modify the contents of the queue
    return (requests ++ [request]) -- Append the new request to the end of the queue and return the updated list
