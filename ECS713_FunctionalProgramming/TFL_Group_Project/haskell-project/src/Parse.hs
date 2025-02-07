-- | Parse a JSON response containing stop points into a list of StopPoint objects.
-- The function validates that the response contains at least one stop point.
--
-- >>> parseStopPoints jsonByteString
-- Right [StopPoint {...}, StopPoint {...}]
--
-- Returns either an error message (Left) or a list of StopPoints (Right)
-- Throws a "No stop points found" error if the response contains an empty list
module Parse (parseStopPoints) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy.Char8 as L8
import Types (StopPoint(..), StopPointResponse(..))
import Control.Monad (when)

parseStopPoints :: L8.ByteString -> Either String [StopPoint]
parseStopPoints body = do
    resp <- eitherDecode body :: Either String StopPointResponse
    let sps = stopPoints resp
    -- Example validation: ensure we have at least one stop point
    if null sps
        then Left "No stop points found in the response."
        else Right sps