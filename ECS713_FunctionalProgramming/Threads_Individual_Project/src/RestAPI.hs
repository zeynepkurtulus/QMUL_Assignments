{-# LANGUAGE OverloadedStrings #-}
{-|
The RestAPI module provides a RESTful API server with endpoints for managing the request queue, adding requests, 
and retrieving logs of processed requests and responses, enabling client interaction with the system.
-}

module RestAPI (startAPI) where

import Web.Scotty
import Control.Concurrent.MVar
import Data.Aeson (ToJSON, encode)
import Types (Request, Response)
import Database (getAllRequests, getAllResponses)

-- Start the REST API server
-- This function initializes the server on port 3000 and defines endpoints for interacting with the system.
startAPI :: MVar [Request] -> FilePath -> IO ()
startAPI requestQueue dbPath = scotty 3000 $ do
    -- Endpoint to get the current state of the queue
    -- This allows clients to view the list of requests currently in the queue.
    get "/queue" $ do
        queue <- liftIO $ readMVar requestQueue -- Read the current state of the request queue in a thread-safe manner
        json queue -- Return the queue as a JSON response

    -- Endpoint to add a new request dynamically
     -- This allows clients to send a request, specifying the `clientId` as a parameter.
    post "/request" $ do
        clientId <- param "clientId" -- Read the `clientId` parameter from the HTTP request
        -- Here, you would create and add the request to the queue
        text $ "Request received from client " <> clientId

    -- Endpoint to get all logs from the database
    -- This fetches all requests and responses stored in the database and returns them as JSON.
    get "/logs" $ do
        requests <- liftIO $ getAllRequests dbPath -- Fetch all requests from the database
        responses <- liftIO $ getAllResponses dbPath -- Fetch all responses from the database
        json (requests, responses) -- Return the requests and responses as a JSON response
