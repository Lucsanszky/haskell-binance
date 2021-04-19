{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}

module Binance.Type
    ( ServerTime(..)
    , AllOrders
    , Order(..)
    , OrderParams(..)
    , BinanceConfig(..)
    , BinanceUserApi(..)
    , TradeParams(..)
    , T(..)
    , WT(..)
    , Side(..)
    , OrderType(..)
    , Response(..)
    , StreamType(..)
    ) where

import           Network.WebSockets (WebSocketsData(..), DataMessage(..))
import           Binance.Prelude
import           Data.Aeson (decode, encode)
import qualified Data.Aeson.Types    as A (Options (..))
import           Data.ByteString     (ByteString)
import           Network.HTTP.Client (Manager)
import           Prelude             hiding (String)


import Debug.Trace

traceme :: Show a => String -> a -> a
traceme s a = a `seq` traceShowPreF s id a

traceShowPreF :: (Show b) => String -> (a -> b) -> a -> a
traceShowPreF prefix f a = trace (prefix ++ show (f a)) a

------------------------------------------------------------
-- BINANCE DATA
--
data BinanceConfig = BinanceConfig
    { url        :: !BaseUrl
    , managr     :: !Manager
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

instance FromJSON Side

instance ToJSON Side

instance ToHttpApiData Side where
    toUrlPiece = pack . show
    toEncodedUrlPiece = unsafeToEncodedUrlPiece

instance FromHttpApiData Side where
    parseUrlPiece "BUY" = Right BUY
    parseUrlPiece "SELL" = Right SELL
    parseUrlPiece _ = Left "Invalid side (should be BUY or SELL)"


data OrderType
    = LIMIT
    | MARKET
    | STOP_LOSS
    | STOP_LOSS_LIMIT
    | TAKE_PROFIT
    | TAKE_PROFIT_LIMIT
    | LIMIT_MAKER
    deriving (Eq, Show, Generic)

instance FromJSON OrderType

instance ToJSON OrderType

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

instance FromJSON Response

instance ToJSON Response

data TradeParams = TradeParams
    { _symbol           :: !Text
    , _side             :: !Side
    , _type             :: !OrderType
    , _quantity         :: !(Maybe Double)
--    , _quoteOrderQty    :: Maybe Double
--    , _timeInForce      :: Maybe Text
--    , _price            :: Maybe Double
--    , _newClientOrderId :: Maybe Text
--    , _stopPrice        :: Maybe Double
--    , _icebergQty       :: Maybe Double
--    , _newOrderRespType :: Maybe Response
--    , _recvWindow       :: Maybe Integer
    , _timestamp        :: !Integer
    } deriving (Eq, Show, Generic)

-- instance FromJSON TradeParams where
--     parseJSON = genericParseJSON $ defaultOptions {A.fieldLabelModifier = drop 1}

-- instance ToJSON TradeParams

instance ToHttpApiData TradeParams where
    toUrlPiece = pack . show
    toEncodedUrlPiece = unsafeToEncodedUrlPiece

--instance FromHttpApiData TradeParams where
--    parseUrlPiece "ACK" = Right ACK
--    parseUrlPiece "RESULT" = Right RESULT
--    parseUrlPiece "FULL" = Right FULL
--    parseUrlPiece _ =
--        Left
--            "Invalid response type (should be ACK, RESULT or FULL)"

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

data T = T
  { symbol :: Text
  , time :: Integer
  , price :: Double
  } deriving (Eq)

instance FromJSON T where
  parseJSON = withObject "T" $ \o ->
    T
      <$> o .: "s"
      <*> o .: "T"
      <*> (read <$> o .: "p")


instance ToJSON T where
  toJSON T{..} =
    object [ "s" .= symbol
           , "T" .= time
           , "p" .= price
           ]

instance WebSocketsData (Maybe T) where
  fromDataMessage (Text s _) = fromLazyByteString s
  fromDataMessage (Binary s) = fromLazyByteString s
  fromLazyByteString = decode
  toLazyByteString = encode

data WT = WT
  { stream  :: Text
  , payload :: T
  } deriving (Eq)

instance FromJSON WT where
  parseJSON = withObject "WT" $ \o ->
    WT
      <$> o .: "stream"
      <*> o .: "data"

instance ToJSON WT where
  toJSON WT{..} =
    object [ "stream" .= stream
           , "data"   .= payload
           ]

instance WebSocketsData (Maybe WT) where
  fromDataMessage (Text s _) = fromLazyByteString s
  fromDataMessage (Binary s) = fromLazyByteString s
  fromLazyByteString = decode
  toLazyByteString = encode

instance Show T where
  show (T s t p) = unpack s ++ " " ++ show t ++ " " ++ show p

instance Read T where
  readsPrec _ s =
    let (ss:ts:ps:_) = words s
     in [(T (pack ss) (read ts) (read ps), "")]

instance Show WT where
  show (WT _ t) = show t

