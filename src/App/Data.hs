module App.Data where

import Data.Aeson (FromJSON)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Lucid (ToHtml)
import Servant.API (FromHttpApiData)

import qualified App.Common as Common
import qualified Lucid as Lucid

newtype ID a = ID { unID :: Int32 }
    deriving newtype (Eq, Show, FromHttpApiData)

instance ToHtml (ID a) where
    toHtml = Lucid.toHtml . Common.showT
    toHtmlRaw = Lucid.toHtml

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
