module App
    ( prodMain
    , develMain
    )
    where

import Data.Proxy
import Network.Wai.Handler.Warp
import Prelude
import System.Environment

import API.Definition (API)
import API.Handler (server)
import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (race_)
import Servant.Server (Application, serve)
import System.Directory         (doesFileExist)

import qualified Hasql.Connection as Hasql


getDBConnection :: Bool -> IO (Either Hasql.ConnectionError Hasql.Connection)
getDBConnection isProd = do
    let dbConnSettings = Hasql.settings "localhost" 5432 "postgres" "dummy" $
            if isProd then "trucker_timecards" else "trucker_timecards_test"
    Hasql.acquire dbConnSettings

application :: Hasql.Connection -> Application
application dbConn = serve @API Proxy $ server dbConn

prodMain :: IO ()
prodMain = do
    connResult <- getDBConnection True
    let port = 8080
    case connResult of
        Left connErr -> print connErr
        Right conn   -> run port $ application conn

develMain :: IO ()
develMain = race_ watchTermFile $ do
    port <- read <$> getEnv "PORT"
    displayPort <- getEnv "DISPLAY_PORT"
    putStrLn $ "Running in development mode on port " ++ show port
    putStrLn $ "But you should connect to port " ++ displayPort

    connResult <- getDBConnection False

    case connResult of
        Left connErr -> print connErr
        Right conn   -> run port $ application conn
        
watchTermFile :: IO ()
watchTermFile =
    loop
  where
    loop = do
        exists <- doesFileExist "yesod-devel/devel-terminate"
        if exists
            then return ()
            else do
                threadDelay 100000
                loop
