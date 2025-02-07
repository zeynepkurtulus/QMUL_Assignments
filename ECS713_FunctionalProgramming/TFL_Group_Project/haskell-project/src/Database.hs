{-# LANGUAGE OverloadedStrings #-}

module Database (
    createDatabase,
    saveStopPointsToDatabase,
    queryStopPoints,
    dumpStopPointsToJSON
) where

import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Types (StopPoint(..))
import Data.String (fromString)
import Control.Monad (forM_)

-- | Create the SQLite database with two tables:
--   stop_points: holds basic stop point info
--   modes: holds modes associated with each stop point
-- A foreign key from `modes.stop_point_id` references `stop_points.id`.
createDatabase :: FilePath -> IO ()
createDatabase dbPath = do
    conn <- SQLite.open dbPath
    -- Create the main stop_points table
    SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS stop_points (\
                         \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                         \ naptanId TEXT NOT NULL, \
                         \ commonName TEXT NOT NULL, \
                         \ lat REAL, \
                         \ lon REAL)"

    -- Create the modes table, linked to stop_points by a foreign key
    SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS modes (\
                         \ id INTEGER PRIMARY KEY AUTOINCREMENT, \
                         \ stop_point_id INTEGER NOT NULL, \
                         \ mode TEXT NOT NULL, \
                         \ FOREIGN KEY(stop_point_id) REFERENCES stop_points(id))"

    putStrLn "Database and tables created."
    SQLite.close conn

-- | Save a list of StopPoints to the database.
-- For each StopPoint:
--   1. Insert into stop_points
--   2. Retrieve the inserted row's ID
--   3. Insert each mode into the modes table, referencing the stop_points row
saveStopPointsToDatabase :: FilePath -> [StopPoint] -> IO ()
saveStopPointsToDatabase dbPath stopPoints = do
    conn <- SQLite.open dbPath
    let insertStopPoint = "INSERT INTO stop_points (naptanId, commonName, lat, lon) VALUES (?, ?, ?, ?)"
    let insertMode = "INSERT INTO modes (stop_point_id, mode) VALUES (?, ?)"

    forM_ stopPoints $ \sp -> do
       SQLite.execute conn insertStopPoint (naptanId sp, commonName sp, lat sp, lon sp)
       stopId <- SQLite.lastInsertRowId conn
       forM_ (modes sp) $ \m -> SQLite.execute conn insertMode (stopId, m)

    putStrLn "StopPoints and their modes saved to database."
    SQLite.close conn

-- | Query StopPoints from the database.
-- Uses GROUP_CONCAT to combine multiple modes into a single string.
queryStopPoints :: FilePath -> String -> IO [StopPoint]
queryStopPoints dbPath condition = do
    conn <- SQLite.open dbPath
    let baseQuery = "SELECT sp.naptanId, sp.commonName, \
                    \ IFNULL(GROUP_CONCAT(m.mode, ' '), '') as modes, \
                    \ sp.lat, sp.lon \
                    \ FROM stop_points sp \
                    \ LEFT JOIN modes m ON m.stop_point_id = sp.id"
    let fullQuery =
            if null condition
            then baseQuery ++ " GROUP BY sp.id"
            else baseQuery ++ " WHERE " ++ condition ++ " GROUP BY sp.id"

    rows <- SQLite.query_ conn (SQLite.Query (fromString fullQuery))
    SQLite.close conn
    return rows

-- | Dump all StopPoints to a JSON file.
-- Similar to queryStopPoints, but no condition. Dumps all stop points with their modes.
dumpStopPointsToJSON :: FilePath -> FilePath -> IO ()
dumpStopPointsToJSON dbPath jsonPath = do
    conn <- SQLite.open dbPath
    let query = "SELECT sp.naptanId, sp.commonName, \
                \ IFNULL(GROUP_CONCAT(m.mode, ' '), '') as modes, \
                \ sp.lat, sp.lon \
                \ FROM stop_points sp \
                \ LEFT JOIN modes m ON m.stop_point_id = sp.id \
                \ GROUP BY sp.id"
    rows <- SQLite.query_ conn (SQLite.Query (fromString query)) :: IO [StopPoint]
    case rows of
        [] -> putStrLn "No data found in the database to dump."
        _  -> do
            let json = encode rows
            B.writeFile jsonPath json
            putStrLn $ "Data dumped to " ++ jsonPath
    SQLite.close conn

-- | FromRow instance to parse database rows into StopPoint.
-- The modes are returned as a single string from GROUP_CONCAT and split by words.
instance FromRow StopPoint where
    fromRow = StopPoint <$> field <*> field <*> (words <$> field) <*> field <*> field