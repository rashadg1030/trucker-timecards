module API.Handler where

import API.Definition
import Servant.API
import Servant.Server
import Servant.Server.StaticFiles

import App.Data (ID, Trucker, TruckerData, TruckerFormData)
import Control.Monad.Except (throwError)
import HTML.Page.DataEntry (DataEntry(..))
import HTML.Page.NameList (NameList(..))
import HTML.Partial.TruckerRow (TruckerRow(..))

import qualified Hasql.Connection as Hasql


getDataEntryScreenHandler :: Hasql.Connection -> Handler DataEntry
getDataEntryScreenHandler _ = pure $ DataEntry Nothing

getNameListScreenHandler :: Hasql.Connection -> Handler NameList
getNameListScreenHandler _ = pure $ NameList []

postTruckerHandler :: Hasql.Connection -> TruckerData -> Handler TruckerRow
postTruckerHandler _ _ = throwError err404

patchTruckerHandler :: Hasql.Connection -> ID Trucker -> TruckerData -> Handler TruckerRow
patchTruckerHandler _ _ _ = throwError err404

deleteTruckerHandler :: Hasql.Connection -> ID Trucker -> Handler NoContent
deleteTruckerHandler _ _ = throwError err404

-- getTruckerFormHandler :: Hasql.Connection -> ID Trucker -> Handler Trucker
-- getTruckerFormHandler _ _ = throwError err404

postTruckerFormDataHandler :: Hasql.Connection -> ID Trucker -> TruckerFormData -> Handler NoContent
postTruckerFormDataHandler _ _ _ = throwError err404

server :: Hasql.Connection -> Server API
server conn = getDataEntryScreenHandler conn
    :<|> getNameListScreenHandler conn
    :<|> postTruckerHandler conn
    :<|> patchTruckerHandler conn
    :<|> deleteTruckerHandler conn
    -- :<|> getTruckerFormHandler conn
    :<|> postTruckerFormDataHandler conn
    :<|> serveDirectoryWebApp "/dist"
