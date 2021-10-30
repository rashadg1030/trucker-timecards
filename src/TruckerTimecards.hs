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

import Navbar (navWithSearch, nav)
import Lucid.Html5 (defer_, disabled_)
import Lucid.Supplemental

newtype ID a = ID { unID :: Int32 }
    deriving newtype (Eq, Show, FromHttpApiData)

instance ToHtml (ID a) where
    toHtml = toHtml . showT

newtype Name = Name { unName :: Text }
    deriving newtype (Eq, Show, FromJSON, ToHtml)

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

data DataEntryScreen = DataEntryScreen (Maybe Trucker)

data TruckerRow = TruckerRow Trucker

data TruckerInputRow = TruckerInputRow (Maybe Trucker)

data NameListScreen = NameListScreen [TruckerRow]

type GetDataEntryScreen = Get '[HTML] DataEntryScreen

type GetNameListScreen = "trucker" :> Get '[HTML] NameListScreen

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

-- type GetTruckerForm = "trucker-form"
--     :> CaptureTruckerID
--     :> Get '[HTML] TruckerFields

type PostTruckerFormData = "trucker-form"
    :> CaptureTruckerID
    :> ReqBody '[JSON] TruckerFormData
    :> Post '[HTML] NoContent

type API = GetDataEntryScreen
    :<|> GetNameListScreen
    :<|> PostTrucker
    :<|> PatchTrucker
    :<|> DeleteTrucker
    -- :<|> GetTruckerForm
    :<|> PostTruckerFormData
    :<|> Raw

getDataEntryScreenHandler :: Connection.Connection -> Handler DataEntryScreen
getDataEntryScreenHandler _ = pure $ DataEntryScreen Nothing

getNameListScreenHandler :: Connection.Connection -> Handler NameListScreen
getNameListScreenHandler _ = pure $ NameListScreen []

postTruckerHandler :: Connection.Connection -> TruckerData -> Handler Trucker
postTruckerHandler _ _ = throwError err404

patchTruckerHandler :: Connection.Connection -> ID Trucker -> TruckerData -> Handler Trucker
patchTruckerHandler _ _ _ = throwError err404

deleteTruckerHandler :: Connection.Connection -> ID Trucker -> Handler NoContent
deleteTruckerHandler _ _ = throwError err404

-- getTruckerFormHandler :: Connection.Connection -> ID Trucker -> Handler Trucker
-- getTruckerFormHandler _ _ = throwError err404

postTruckerFormDataHandler :: Connection.Connection -> ID Trucker -> TruckerFormData -> Handler NoContent
postTruckerFormDataHandler _ _ _ = throwError err404

server :: Connection.Connection -> Server API
server conn = getDataEntryScreenHandler conn
    :<|> getNameListScreenHandler conn
    :<|> postTruckerHandler conn
    :<|> patchTruckerHandler conn
    :<|> deleteTruckerHandler conn
    -- :<|> getTruckerFormHandler conn
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

    html_ [lang_ "en"] $ do

        head_ $ do
            meta_ [charset_ "utf-8"]
            meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]

            title_ $ toHtml title

            link_ [href_ "https://unpkg.com/tailwindcss@^2/dist/tailwind.min.css", rel_ "stylesheet"]
            script_ [src_ "https://unpkg.com/alpinejs@3.4.2/dist/cdn.min.js", defer_ ""] noHtml
            script_ [src_ "https://unpkg.com/htmx.org@1.5.0"] noHtml
            script_ [src_ "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] noHtml

        body_ [id_ "main-content"] innerHtml

instance ToHtml DataEntryScreen where
    toHtml (DataEntryScreen mbTrucker) = baseTemplate "Trucker Timecards" $ do
        navWithSearch
        div_ [class_ "m-10"] $ do
            case mbTrucker of
                Nothing -> do
                    h1_ [class_ "text-xl font-semibold"] "Use the search bar above to select a trucker for data entry."
                Just trucker -> do
                    h1_ [] $ toHtml $ "Trucker " <> showT (truckerName trucker) <> " found"

instance ToHtml TruckerInputRow where
    toHtml (TruckerInputRow _) = do
        -- let rowId = "trucker-row-" <> showT (truckerID trucker)

        tr_ [id_ "777"] $ do
            td_ [tableCellCss_ ""] "777"
            td_ [tableCellCss_ ""] "John Doe"

            disabledToggle_ $ False
            disabledToggle_ $ False
            disabledToggle_ $ False
            disabledToggle_ $ False
            disabledToggle_ $ False
            disabledToggle_ $ False
            disabledToggle_ $ False
            disabledToggle_ $ False

            td_ [tableCellCss_ ""] $ do
                span_ [class_ "flex flex-row justify-center align-middle"] $ do
                    button_
                        [ buttonCss_ "mr-2 bg-purple-400"
                        -- , hxGetSafe_ $ getContactFormLink cID
                        -- , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Edit"
                    button_
                        [ buttonCss_ "bg-red-400"
                        -- , hxDeleteSafe_ $ deleteContactLink cID
                        , hxConfirm_ "Are you sure?"
                        -- , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Delete"
        

instance ToHtml NameListScreen where
    toHtml (NameListScreen truckerRows) = do
        nav
        div_ [class_ "m-10"] $ do
            table_ [class_ "table-auto rounded-lg"] $ do
                thead_ [] $
                    tr_ [] $ do
                        th_ [tableHeaderCss_ ""] "ID"
                        th_ [tableHeaderCss_ ""] "Name"
                        th_ [tableHeaderCss_ ""] "RR2"
                        th_ [tableHeaderCss_ ""] "THE"
                        th_ [tableHeaderCss_ ""] "REG1"
                        th_ [tableHeaderCss_ ""] "REG2"
                        th_ [tableHeaderCss_ ""] "MILES"
                        th_ [tableHeaderCss_ ""] "PTP"
                        th_ [tableHeaderCss_ ""] "BLU"
                        th_ [tableHeaderCss_ ""] "TRG"
                        th_ [tableHeaderCss_ ""] "Actions"
                tbody_ $ do
                    Prelude.mapM_ toHtml truckerRows
                    toHtml $ TruckerInputRow Nothing

buttonCss_ :: Text -> Attribute
buttonCss_ custom = class_ $
    "px-4 py-2 text-lg text-white rounded-md " <> custom

tableCellCss_ :: Text -> Attribute
tableCellCss_ custom = class_ $
    "border-4 items-center justify-center px-4 py-2 text-semibold text-lg text-center " <> custom

tableHeaderCss_ :: Text -> Attribute
tableHeaderCss_ = tableCellCss_

disabledToggle_ :: Monad m => Bool -> HtmlT m ()
disabledToggle_ bool = td_ [tableCellCss_ ""] $
    button_
        [ type_ "button"
        , class_ "bg-gray-200 relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
        , role_ "switch"
        , disabled_ ""
        ] $ do
            span_ [class_ "sr-only"] "Use setting"
            span_ [class_ $ if bool then "translate-x-5" else "translate-x-0" <> " pointer-events-none relative inline-block h-5 w-5 rounded-full bg-white shadow ring-0"] $
                span_ [class_ "absolute inset-0 h-full w-full flex items-center justify-center"] $
                    if bool
                    then do
                        svg_ [class_ "h-3 w-3 text-indigo-600", fill_ "currentColor", viewBox_ "0 0 12 12"] $
                            path_ [d_ "M3.707 5.293a1 1 0 00-1.414 1.414l1.414-1.414zM5 8l-.707.707a1 1 0 001.414 0L5 8zm4.707-3.293a1 1 0 00-1.414-1.414l1.414 1.414zm-7.414 2l2 2 1.414-1.414-2-2-1.414 1.414zm3.414 2l4-4-1.414-1.414-4 4 1.414 1.414z"]
                    else do
                        svg_ [class_ "h-3 w-3 text-gray-400", fill_ "none", viewBox_ "0 0 12 12"] $
                            path_ [d_ "M4 8l2-2m0 0l2-2M6 6L4 4m2 2l2 2", stroke_ "currentColor", strokeWidth_ "2", strokeLinecap_ "round", strokeLinejoin_ "round"]

    {-
        <button type="button" class="bg-gray-200 relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500" role="switch" aria-checked="false">
            <span class="sr-only">Use setting</span>
            <!-- Enabled: "translate-x-5", Not Enabled: "translate-x-0" -->
            <span class="translate-x-0 pointer-events-none relative inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200">
                <!-- Enabled: "opacity-0 ease-out duration-100", Not Enabled: "opacity-100 ease-in duration-200" -->
                <span class="opacity-100 ease-in duration-200 absolute inset-0 h-full w-full flex items-center justify-center transition-opacity" aria-hidden="true">
                    <svg class="h-3 w-3 text-gray-400" fill="none" viewBox="0 0 12 12">
                        <path d="M4 8l2-2m0 0l2-2M6 6L4 4m2 2l2 2" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
                    </svg>
                </span>
                <!-- Enabled: "opacity-100 ease-in duration-200", Not Enabled: "opacity-0 ease-out duration-100" -->
                <span class="opacity-0 ease-out duration-100 absolute inset-0 h-full w-full flex items-center justify-center transition-opacity" aria-hidden="true">
                    <svg class="h-3 w-3 text-indigo-600" fill="currentColor" viewBox="0 0 12 12">
                        <path d="M3.707 5.293a1 1 0 00-1.414 1.414l1.414-1.414zM5 8l-.707.707a1 1 0 001.414 0L5 8zm4.707-3.293a1 1 0 00-1.414-1.414l1.414 1.414zm-7.414 2l2 2 1.414-1.414-2-2-1.414 1.414zm3.414 2l4-4-1.414-1.414-4 4 1.414 1.414z" />
                    </svg>
                </span>
            </span>
        </button>
    -}


instance ToHtml TruckerRow where
    toHtml (TruckerRow trucker) = do
        let rowId = "trucker-row-" <> showT (truckerID trucker)

        tr_ [id_ rowId] $ do
            td_ [tableCellCss_ ""] $ toHtml $ truckerID trucker
            td_ [tableCellCss_ ""] $ toHtml $ truckerName trucker

            let fields = truckerFields trucker

            disabledToggle_ $ truckerFieldsRR2 fields
            disabledToggle_ $ truckerFieldsTHE fields
            disabledToggle_ $ truckerFieldsREG1 fields
            disabledToggle_ $ truckerFieldsREG2 fields
            disabledToggle_ $ truckerFieldsMILES fields
            disabledToggle_ $ truckerFieldsPTP fields
            disabledToggle_ $ truckerFieldsBLU fields
            disabledToggle_ $ truckerFieldsTRG fields

            td_ [tableCellCss_ ""] $ do
                span_ [class_ "flex flex-row justify-center align-middle"] $ do
                    button_
                        [ buttonCss_ "mr-2 bg-purple-400"
                        -- , hxGetSafe_ $ getContactFormLink cID
                        , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Edit"
                    button_
                        [ buttonCss_ "bg-red-400"
                        -- , hxDeleteSafe_ $ deleteContactLink cID
                        , hxConfirm_ "Are you sure?"
                        , hxTarget_ $ "#" <> rowId
                        , hxSwap_ "outerHTML"
                        ]
                        "Delete"

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
