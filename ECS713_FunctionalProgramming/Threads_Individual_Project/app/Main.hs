{-|
Main module is the entry point for the application. It initializes the server, clients, and REST API.
-}
module Main where

import Database (getAllRequests, getAllResponses, initializeDatabase)
import Control.Concurrent (forkIO, newMVar, threadDelay)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import System.IO (writeFile)
import Types (Request(..), RequestQueue, Response(..))
import Client (sendRequest)
import Server (serverLoop, writeLog)
import RestAPI (startAPI)
import Data.Time.Clock (getCurrentTime)
import Control.Monad (when)

main :: IO ()
main = do
    putStrLn "Starting the server, clients, and REST API..." -- for debugging purpuses 

    -- Database file path
    let dbPath = "simulation.db"

    -- Initialize the database
    initializeDatabase dbPath

    -- Shared resources
    requestQueue <- newMVar []       -- Request queue
    counter      <- newMVar 0        -- Request ID counter
    dbLock       <- newMVar ()       -- Lock for database access

    -- Start the server
    _ <- forkIO $ serverLoop requestQueue dbPath dbLock

    -- Start the REST API
    _ <- forkIO $ startAPI requestQueue dbPath

    -- Start 10 clients
    mapM_ (\clientId -> forkIO $ clientLoop counter requestQueue dbPath dbLock clientId) [1..10]

    -- Wait for all requests to be sent
    let waitUntilAllRequestsSent = do
          currentCount <- readMVar counter  -- Read the current value of the `counter` MVar (number of requests sent)
          if currentCount < 100 -- Check if the total requests sent are less than 100
              then threadDelay 100000 >> waitUntilAllRequestsSent  -- If less than 100, pause for 0.1 seconds. After pausing, recursively call the function to check again
              else return () -- If 100 requests have been sent, terminate the function
    waitUntilAllRequestsSent  -- Call the function to wait for all requests to be sent

    -- Wait for all requests to be processed
    let waitUntilAllResponsesSaved = do
          requests <- getAllRequests dbPath -- Fetch all requests from the database
          responses <- getAllResponses dbPath -- Fetch all responses from the database
          when (length responses < length requests) $ do  -- Check if the number of responses is less than the number of requests
              threadDelay 100000   -- If so, wait for 0.1 seconds
              waitUntilAllResponsesSaved -- Recursively call the function to check again
    waitUntilAllResponsesSaved -- Call the function to wait for all responses to be saved

    -- Fetch all requests and responses from the database
    requests <- getAllRequests dbPath
    responses <- getAllResponses dbPath

    -- Save the request-response log to a JSON file
    writeLog "requests.log" (zip requests responses)

    -- For debugging purposes
    putStrLn "Simulation complete. Logs saved to 'requests.json'."
    putStrLn "REST API is still running. Access the API endpoints for more information."

-- | Simulates a client sending requests until 100 total requests are sent.
clientLoop :: MVar Int -> RequestQueue -> FilePath -> MVar () -> Int -> IO ()
clientLoop counter queue dbPath dbLock clientId = do
    -- Generate and send a new request
    _ <- sendRequest counter queue clientId dbPath dbLock

    -- Check if 100 requests have been sent
    currentCount <- readMVar counter -- Read the current value of the counter
    if currentCount >= 100 -- Check if 100 requests have been sent
        then return ()  -- If so, stop the loop
        else clientLoop counter queue dbPath dbLock clientId -- Otherwise, call the function recursively to send more requests

-- | Appends a request to the shared request log.
appendRequestLog :: MVar [Request] -> Request -> IO ()
appendRequestLog requestList request = modifyMVar_ requestList $ \log -> do -- Use `modifyMVar_` to safely modify the contents of the MVar
    return (request : log) -- Append the new request to the front of the log and return the updated log

-- | Saves the request-response log to a JSON file.
--
-- * 'requestList' - Shared list of sent requests.
-- * 'responseLog' - Shared list of responses.
saveLog :: MVar [Request] -> MVar [Response] -> IO ()
saveLog requestList responseLog = do
    requests  <- readMVar requestList
    responses <- readMVar responseLog
    let pairedLog = zip (reverse requests) (reverse responses)  -- Pair requests and responses
    writeLog "requests.log" pairedLog
