{-# LANGUAGE OverloadedStrings #-}

{-|
Database module implements database operations for saving and retrieving requests and responses.
-}
module Database
  ( initializeDatabase
  , saveRequest
  , saveResponse
  , getAllRequests
  , getAllResponses
  ) where

import Database.SQLite.Simple
import Control.Exception (try, SomeException)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime)
import Types (Request(..), Response(..))




-- | Retry logic for SQLite operations.
withRetry :: IO () -> IO ()
withRetry action = go 5  -- Retry up to 5 times
  where
    go 0 = putStrLn "Operation failed after retries." -- If all retries are exhausted, print a failure message
    go n = do
      result <- try action :: IO (Either SomeException ()) -- Attempt the action and catch any exceptions
      case result of
        Right _ -> return () -- If the action succeeds, terminate the retry logic
        Left _  -> do
          putStrLn "Database is busy. Retrying..." -- If the action fails, print a retry message
          threadDelay 100000  -- Wait 0.1 seconds before retrying
          go (n - 1) -- Decrease the retry count and attempt the action again


-- | Initializes the database by creating the necessary tables.
initializeDatabase :: FilePath -> IO ()
initializeDatabase dbPath = do
    putStrLn "Initializing database..."
    conn <- open dbPath -- Open a connection to the database at the specified file path
    -- Execute an SQL statement to create the `requests` table if it doesn't already exist
    execute_ conn "CREATE TABLE IF NOT EXISTS requests (id INTEGER PRIMARY KEY, time TEXT, content TEXT)" 
     -- Execute an SQL statement to create the `responses` table if it doesn't already exist
    execute_ conn "CREATE TABLE IF NOT EXISTS responses (id INTEGER PRIMARY KEY, time TEXT, content TEXT)"
     -- Print a message to indicate the database initialization process has completed
    putStrLn "Database initialized."
    close conn -- Close the connection to the database

-- | Saves a request to the database with retry logic.
saveRequest :: FilePath -> Request -> IO ()
saveRequest dbPath (Request reqId reqTime reqContent) = withRetry $ do
    putStrLn $ "Saving request: " ++ show (reqId, reqTime, reqContent) -- Debugging Mesaages 
    conn <- open dbPath -- Open a connection to the database
    execute conn "INSERT INTO requests (id, time, content) VALUES (?, ?, ?)"  -- Execute an SQL statement to insert the request into the `requests` table
        (reqId, show reqTime, reqContent) -- Bind the request's ID, timestamp, and content to the SQL statement
    putStrLn $ "Request with ID " ++ show reqId ++ " saved." -- Print a success message indicating the request was saved for debugging 
    close conn -- Close the database connection

-- | Saves a response to the database with retry logic.
saveResponse :: FilePath -> Response -> IO ()
saveResponse dbPath (Response resId resTime resContent) = withRetry $ do
    putStrLn $ "Saving response: " ++ show (resId, resTime, resContent) -- Print a message indicating the response being saved for debugging 
    conn <- open dbPath -- Open a connection to the database
    execute conn "INSERT INTO responses (id, time, content) VALUES (?, ?, ?)"   -- Execute an SQL statement to insert the response into the `responses` table
        (resId, show resTime, resContent)  -- Bind the response's ID, timestamp, and content to the SQL statement
    putStrLn $ "Response with ID " ++ show resId ++ " saved."  -- Print a success message indicating the response was saved
    close conn -- Close the database connection

-- | Retrieves all requests from the database.
getAllRequests :: FilePath -> IO [Request]
getAllRequests dbPath = do
    putStrLn "Fetching all requests from the database..."
    conn <- open dbPath
    rows <- query_ conn "SELECT id, time, content FROM requests" :: IO [(Int, String, String)]
    close conn
    putStrLn $ "Fetched " ++ show (length rows) ++ " requests."
    return $ map (\(id, time, content) -> Request id (read time) content) rows

-- | Retrieves all responses from the database.
getAllResponses :: FilePath -> IO [Response]
getAllResponses dbPath = do
    putStrLn "Fetching all responses from the database..." -- Print a message indicating the retrieval process has started for debugging 
    conn <- open dbPath -- Open a connection to the database
    rows <- query_ conn "SELECT id, time, content FROM responses" :: IO [(Int, String, String)] -- Execute an SQL query to fetch all rows from the `responses` table, fetch `id`, `time`, and `content` columns from the table, specify the expected result as a list of tuples (Int, String, String)
    close conn -- Close the database connection
    putStrLn $ "Fetched " ++ show (length rows) ++ " responses."
    -- Map over the retrieved rows to convert each tuple into a `Response` object
    -- Parse the `time` field into the appropriate type and construct `Response` objects
    return $ map (\(id, time, content) -> Response id (read time) content) rows 
