module TruckerTimecards
    ( prodMain
    , develMain
    )
    where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.Int
import Data.Profunctor
import Data.Proxy
import Data.Text
import Data.Time.Clock
import Data.Vector (Vector)
import GHC.Generics
import Hasql.TH
import Hasql.Session (Session, QueryError)
import Hasql.Statement (Statement(..))
import Lucid
import Lucid.Base (makeAttribute)
import Lucid.HTMX
import Lucid.HTMX.Servant
import Network.Wai.Handler.Warp
import Prelude
import Servant.API
import Servant.HTML.Lucid
import Servant.Server
import Servant.Server.StaticFiles

import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Hasql.Session as Session
import qualified Hasql.Connection as Connection
import Data.Data (Data)

import Control.Concurrent       (threadDelay)
import Control.Concurrent.Async (race_)
import System.Directory         (doesFileExist)
import System.Environment

import Navbar (template1)

newtype ID a = ID { unID :: Int32 }
    deriving newtype (Eq, Show, FromHttpApiData)

newtype Name = Name { unName :: Text }
    deriving newtype (Eq, Show, FromJSON)

data TruckerFields = TruckerFields
    { truckerFieldsRR2 :: Bool
    , truckerFieldsTHE :: Bool
    , truckerFieldsREG1 :: Bool
    , truckerFieldsREG2 :: Bool
    , truckerFieldsMILES :: Bool
    , truckerFieldsPTP :: Bool
    , truckerFieldsBLU :: Bool
    , truckerFieldsTRG :: Bool
    }
    deriving (Eq, Show, Generic, FromJSON)

data Trucker = Trucker
    { truckerID :: ID Trucker
    , truckerName :: Name
    , truckerFields :: TruckerFields
    }
    deriving (Eq, Show)

data TruckerData = TruckerData
    { truckerDataName :: Name
    , truckerDataFields :: TruckerFields
    }
    deriving (Eq, Show, Generic, FromJSON)

data RR2 = RR2
    { rr2StartTime :: Double
    , rr2EndTime :: Double
    , rr2Comment :: Text
    }
    deriving (Eq, Show, Generic, FromJSON)

data THE = THE
    { theStartTime :: Double
    , theEndTime :: Double
    , theComment :: Text
    }
    deriving (Eq, Show, Generic, FromJSON)

data REG1 = REG1
    { reg1StartTime :: Double
    , reg1EndTime :: Double
    }
    deriving (Eq, Show, Generic, FromJSON)

data REG2BRD = REG2BRD
    { reg2BRDFst :: Double
    , reg2BRDSnd :: Double
    , reg2BRDComment :: Text
    }
    deriving (Eq, Show, Generic, FromJSON)

data REG2STN = REG2STN
    { reg2STNFst :: Double
    , reg2STNSnd :: Double
    , reg2STNComment :: Text
    }
    deriving (Eq, Show, Generic, FromJSON)

data REG2 = REG2
    { reg2Hourly :: (Double, Double, Double, Double, Double)
    , reg2BRD :: (Double, Double, Text)
    , reg2STN :: (Double, Double, Text)
    , reg2SBS :: Double
    }
    deriving (Eq, Show, Generic, FromJSON)

data MILES = MILES
    { milesStart :: Double
    , milesEnd :: Double
    }
    deriving (Eq, Show, Generic, FromJSON)

newtype PTP = PTP { unPTP :: Int32 }
    deriving newtype (Eq, Show, FromJSON)

data BLU = BLU
    { bluLDA :: Double
    , bluUNL :: Double
    }
    deriving (Eq, Show, Generic, FromJSON)

data TRG = TRG
    { trgTrainees :: Int
    , trgComment :: Text
    }
    deriving (Eq, Show, Generic, FromJSON)

data TruckerFormData = TruckerFormData
    { truckerFormDataDATE :: UTCTime
    , truckerFormDataRR2 :: Maybe RR2
    , truckerFormDataTHE :: Maybe THE
    , truckerFormDataREG1 :: Maybe REG1
    , truckerFormDataREG2 :: Maybe REG2
    , truckerFormDataMILES :: Maybe MILES
    , truckerFormDataPTP :: Maybe PTP
    , truckerFormDataBLU :: Maybe BLU
    , truckerFormDataTRG :: Maybe TRG
    }
    deriving (Eq, Show, Generic, FromJSON)

data DataEntryScreen = DataEntryScreen

type GetDataEntryScreen = Get '[HTML] DataEntryScreen

type GetTruckers = "trucker" :> Get '[HTML] [Trucker]

type CaptureTruckerID = Capture "trucker-id" (ID Trucker)

type PostTrucker = "trucker"
    :> ReqBody '[JSON] TruckerData
    :> Post '[HTML] Trucker

type PatchTrucker = "trucker"
    :> CaptureTruckerID
    :> ReqBody '[JSON] TruckerData
    :> Patch '[HTML] Trucker

type DeleteTrucker = "trucker"
    :> CaptureTruckerID
    :> Delete '[HTML] NoContent

type GetTruckerForm = "trucker-form"
    :> CaptureTruckerID
    :> Get '[HTML] TruckerFields

type PostTruckerFormData = "trucker-form"
    :> CaptureTruckerID
    :> ReqBody '[JSON] TruckerFormData
    :> Post '[HTML] NoContent

type API = GetDataEntryScreen
    :<|> GetTruckers
    :<|> PostTrucker
    :<|> PatchTrucker
    :<|> DeleteTrucker
    :<|> GetTruckerForm
    :<|> PostTruckerFormData
    :<|> Raw

getDataEntryScreenHandler :: Connection.Connection -> Handler DataEntryScreen
getDataEntryScreenHandler _ = pure DataEntryScreen

getTruckersHandler :: Connection.Connection -> Handler [Trucker]
getTruckersHandler _ = pure []

postTruckerHandler :: Connection.Connection -> TruckerData -> Handler Trucker
postTruckerHandler _ _ = throwError err404

patchTruckerHandler :: Connection.Connection -> ID Trucker -> TruckerData -> Handler Trucker
patchTruckerHandler _ _ _ = throwError err404

deleteTruckerHandler :: Connection.Connection -> ID Trucker -> Handler NoContent
deleteTruckerHandler _ _ = throwError err404

getTruckerFormHandler :: Connection.Connection -> ID Trucker -> Handler TruckerFields
getTruckerFormHandler _ _ = throwError err404

postTruckerFormDataHandler :: Connection.Connection -> ID Trucker -> TruckerFormData -> Handler NoContent
postTruckerFormDataHandler _ _ _ = throwError err404

server :: Connection.Connection -> Server API
server conn = getDataEntryScreenHandler conn
    :<|> getTruckersHandler conn
    :<|> postTruckerHandler conn
    :<|> patchTruckerHandler conn
    :<|> deleteTruckerHandler conn
    :<|> getTruckerFormHandler conn
    :<|> postTruckerFormDataHandler conn
    :<|> serveDirectoryWebApp "/dist"

showT :: Show a => a -> Text
showT = Text.pack . show

showB :: Show a => a -> ByteStringLazy.ByteString
showB = ByteStringLazy.fromStrict . ByteStringChar8.pack . show

readT :: Read a => Text -> a
readT = read . Text.unpack

noHtml :: Html ()
noHtml = ""

baseTemplate :: Monad m => Text -> HtmlT m a -> HtmlT m a
baseTemplate title innerHtml = do
    doctype_

    html_ [lang_ "en"] ""

    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

        title_ $ toHtml title

        link_ [href_ "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css", rel_ "stylesheet"]
        script_ [src_ "https://unpkg.com/htmx.org@1.5.0"] noHtml
        script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] noHtml

    body_ innerHtml

instance ToHtml DataEntryScreen where
    toHtml _ = baseTemplate "Trucker Timecards" $ do
        template1

instance ToHtml [Trucker] where

instance ToHtml Trucker where

instance ToHtml TruckerFields where

getDBConnection :: Bool -> IO (Either Connection.ConnectionError Connection.Connection)
getDBConnection isProd = do
    let dbConnSettings = Connection.settings "localhost" 5432 "postgres" "dummy" $
            if isProd then "trucker_timecards" else "trucker_timecards_test"
    Connection.acquire dbConnSettings

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
