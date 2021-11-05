module API.Definition where

import Servant.API
import Servant.HTML.Lucid

import App.Data (ID, Trucker, TruckerFormData, TruckerData)
import HTML.Page.DataEntry (DataEntry)
import HTML.Page.NameList (NameList) -- (ID, Nae,Trucker, TruckerData, TruckerFormData)
import HTML.Partial.TruckerRow (TruckerRow)

type GetDataEntryScreen = Get '[HTML] DataEntry

type GetNameListScreen = "trucker" :> Get '[HTML] NameList

type CaptureTruckerID = Capture "trucker-id" (ID Trucker)

type PostTrucker = "trucker"
    :> ReqBody '[JSON] TruckerData
    :> Post '[HTML] TruckerRow

type PatchTrucker = "trucker"
    :> CaptureTruckerID
    :> ReqBody '[JSON] TruckerData
    :> Patch '[HTML] TruckerRow

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
