{-# LANGUAGE DuplicateRecordFields #-}

module Binance.Type
    ( ServerTime(..)
    , AllOrders
    , Order(..)
    , OrderParams(..)
    , BinanceConfig(..)
    , BinanceUserApi(..)
    , TradeParams(..)
    , Side(..)
    , OrderType(..)
    , Response(..)
    , StreamType(..)
    ) where

import           Binance.Prelude
import qualified Data.Aeson.Types    as A (Options (..))
import           Data.ByteString     (ByteString)
import           Network.HTTP.Client (Manager)
import           Prelude             hiding (String)

------------------------------------------------------------
-- BINANCE DATA
--
data BinanceConfig = BinanceConfig
    { url        :: !BaseUrl
    , manager    :: !Manager
    , publicKey  :: !Text
    , privateKey :: !ByteString
    }

newtype BinanceUserApi a = BinanceUserApi
    { api :: (ReaderT BinanceConfig IO) a
    } deriving ( Applicative
               , Functor
               , Monad
               , MonadIO
               , MonadReader BinanceConfig
               )

newtype ServerTime = ServerTime
    { serverTime :: Integer
    } deriving (Eq, Show, Generic)

instance FromJSON ServerTime

instance ToJSON ServerTime

data OrderParams = OrderParams
    { _symbol     :: !Text
    , _orderId    :: Maybe Integer
    , _limit      :: Maybe Int
    , _recvWindow :: Maybe Integer
    , _timestamp  :: !Integer
    } deriving (Eq, Show, Generic)

instance ToForm OrderParams where
    toForm = genericToForm opts
      where
        opts = FormOptions {fieldLabelModifier = drop 1}

data Order = Order
    { _symbol        :: !Text
    , _orderId       :: !Int
    , _clientOrderId :: !Text
    , _price         :: !Text
    , _origQty       :: !Text
    , _executedQty   :: !Text
    , _status        :: !Text
    , _timeInForce   :: !Text
    , _type          :: !Text
    , _side          :: !Text
    , _stopPrice     :: !Text
    , _icebergQty    :: !Text
    , _time          :: !Integer
    , _isWorking     :: !Bool
    } deriving (Eq, Show, Generic)

instance FromJSON Order where
    parseJSON = genericParseJSON $ defaultOptions {A.fieldLabelModifier = drop 1}

instance ToJSON Order

type AllOrders = [Order]

data Side
    = BUY
    | SELL
    deriving (Eq, Show, Generic)

instance ToHttpApiData Side where
    toUrlPiece = pack . show
    toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance FromHttpApiData Side where
    parseUrlPiece "BUY" = Right BUY
    parseUrlPiece "SELL" = Right SELL
    parseUrlPiece _ = Left "Invalid side (should be BUY or SELL)"

instance ToJSON Side

data OrderType
    = LIMIT
    | MARKET
    | STOP_LOSS
    | STOP_LOSS_LIMIT
    | TAKE_PROFIT
    | TAKE_PROFIT_LIMIT
    | LIMIT_MAKER
    deriving (Eq, Show, Generic)

instance ToHttpApiData OrderType where
    toUrlPiece = pack . show
    toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance FromHttpApiData OrderType where
    parseUrlPiece "LIMIT" = Right LIMIT
    parseUrlPiece "MARKET" = Right MARKET
    parseUrlPiece "STOP_LOSS" = Right STOP_LOSS
    parseUrlPiece "STOP_LOSS_LIMIT" = Right STOP_LOSS_LIMIT
    parseUrlPiece "TAKE_PROFIT" = Right TAKE_PROFIT
    parseUrlPiece "TAKE_PROFIT_LIMIT" = Right TAKE_PROFIT
    parseUrlPiece "LIMIT_MAKER" = Right LIMIT_MAKER
    parseUrlPiece _ = Left "Invalid order type"

instance ToJSON OrderType

data Response
    = ACK
    | RESULT
    | FULL
    deriving (Eq, Show, Generic)

instance ToHttpApiData Response where
    toUrlPiece = pack . show
    toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance FromHttpApiData Response where
    parseUrlPiece "ACK" = Right ACK
    parseUrlPiece "RESULT" = Right RESULT
    parseUrlPiece "FULL" = Right FULL
    parseUrlPiece _ =
        Left
            "Invalid response type (should be ACK, RESULT or FULL)"

instance ToJSON Response

data TradeParams = TradeParams
    { _symbol           :: !Text
    , _side             :: !Side
    , _type             :: !OrderType
    , _timeInForce      :: Maybe Text
    , _quantity         :: !Double
    , _price            :: Maybe Double
    , _newClientOrderId :: Maybe Text
    , _stopPrice        :: Maybe Double
    , _icebergQty       :: Maybe Double
    , _newOrderRespType :: Maybe Response
    , _recvWindow       :: Maybe Integer
    , _timestamp        :: !Integer
    } deriving (Eq, Show, Generic)

instance ToForm TradeParams where
    toForm = genericToForm opts
      where
        opts = FormOptions {fieldLabelModifier = drop 1}

instance FromForm TradeParams

data StreamType
    = AggTrade
    | Trade
    | Ticker
    | Depth
    deriving (Eq, Generic)

instance Show StreamType where
    show AggTrade = "@aggTrade"
    show Trade    = "@trade"
    show Ticker   = "@ticker"
    show Depth    = "@depth"
