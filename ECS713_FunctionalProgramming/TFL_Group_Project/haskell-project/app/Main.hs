{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.IO (hSetBuffering, stdout, BufferMode (NoBuffering))
import Fetch (fetchStopPoints)
import Parse (parseStopPoints)
import Database (createDatabase, saveStopPointsToDatabase, dumpStopPointsToJSON, queryStopPoints)

-- | Main function to handle different commands.
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case args of
    ["create"] -> do
      let dbPath = "stop_points.db"
      putStrLn "Creating SQLite database and tables..."
      createDatabase dbPath
      putStrLn "Database and tables created successfully."

    {-|
      Handles the 'loaddata' command which fetches and stores stop points data from the TfL API.

      The function will:
        1. Construct API URL with provided/default parameters
        2. Fetch stop points data from TfL API
        3. Parse the JSON response
        4. Save valid stop points to a SQLite database
    -}
    ("loaddata":rest) -> do
      let defaultLat = "51.50716"
          defaultLon = "-0.12673"
          defaultRadius = "500"
          defaultModes = "bus"
          defaultStopTypes = "NaptanPublicBusCoachTram"

      let (lat, lon, radius, modes, stopTypes) = case rest of
            [] -> (defaultLat, defaultLon, defaultRadius, defaultModes, defaultStopTypes)
            [l, o, r, m, s] -> (l, o, r, m, s)
            _ -> error "Usage: stack run -- loaddata [lat lon radius modes stopTypes]"

      let url = "https://api.tfl.gov.uk/StopPoint?lat=" ++ lat
                ++ "&lon=" ++ lon
                ++ "&radius=" ++ radius
                ++ "&modes=" ++ modes
                ++ "&stopTypes=" ++ stopTypes
      putStrLn $ "Fetching StopPoints from API using: " ++ url
      response <- fetchStopPoints url
      case response of
        Left err -> putStrLn $ "Failed to fetch data: " ++ err
        Right body -> do
          putStrLn "Raw API response:"
          putStrLn (take 500 $ show body)  -- Show a snippet of the response for debugging
          putStrLn "Parsing JSON..."
          case parseStopPoints body of
            Left parseErr -> putStrLn $ "Failed to parse JSON: " ++ parseErr
            Right stopPoints -> do
              if null stopPoints
                then putStrLn "No valid StopPoints to save."
                else do
                  putStrLn "Parsed StopPoints:"
                  mapM_ print stopPoints
                  let dbPath = "stop_points.db"
                  putStrLn "Saving data to database..."
                  saveStopPointsToDatabase dbPath stopPoints
                  putStrLn "Data saved successfully."

    {-|
      Handles the "dumpdata" command by exporting the database contents to JSON.
      Uses the specified database path and outputs to 'dump.json'.
    -}
    ["dumpdata"] -> do
      let dbPath = "stop_points.db"  -- ^ Path to the SQLite database
      dumpStopPointsToJSON dbPath "dump.json"  -- ^ Export database contents to JSON file
      putStrLn "Data dumped to 'dump.json'."

    {-|
      Handles the "query" command by querying the SQLite database for stop points.
    -}
    ("query" : condition) -> do
      let dbPath = "stop_points.db"
      putStrLn $ "Querying database with condition: " ++ unwords condition
      results <- queryStopPoints dbPath (unwords condition)
      putStrLn "Query Results:"
      mapM_ print results

    {-|
      Command-line usage information for the application.
      Displays available commands and their syntax:
    -}
    _ -> do
      putStrLn "Usage:"
      putStrLn "  stack run -- create"
      putStrLn "  stack run -- loaddata <latitude> <longitude> <radius> <modes> <stopTypes>"
      putStrLn "  stack run -- dumpdata"
      putStrLn "  stack run -- query <SQL_condition>"


      -- Example usage of stack run -- query 
      -- stack run -- query "lat > 51.508"
      -- stack run -- query "lon < -0.126"
      -- stack run -- query "commonName LIKE '%Station%' AND lon > -0.125"