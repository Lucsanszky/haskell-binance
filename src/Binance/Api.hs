{-# LANGUAGE NoImplicitPrelude #-}

module Binance.Api
    ( module Binance.Type
    , allOrders
    , BinanceAccountApi
    , binanceStream
    , binanceProxy
    , getServerTime
    , testOrder
    ) where

import Binance.Prelude
import Binance.Type (StreamType, ServerTime(..), TestOrderParams(..),
                     OrderParams(..), BinanceUserApi,
                     publicKey, privateKey, url, managr, BinanceConfig(..), api, Side(..), OrderType(..), OrderResponse(..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.List (intercalate)
import Prelude hiding (getLine, null, putStrLn, readFile)

-- import Debug.Trace
-- 
-- traceme :: Show a => String -> a -> a
-- traceme s a = a `seq` traceShowPreF s id a
-- 
-- traceShowPreF :: (Show b) => String -> (a -> b) -> a -> a
-- traceShowPreF prefix f a = trace (prefix ++ show (f a)) a


subscribeTo :: String -> ClientApp () -> IO ()
subscribeTo s = withSocketsDo .  runSecureClient "stream.binance.com" 9443 s

makeStreamName :: [(String, StreamType)] -> String
makeStreamName ps = base ++
    intercalate "/" (map (\(sym, t) -> map toLower sym ++ show t) ps)
  where
    base =
        if length ps == 1
            then "/ws/"
            else "/stream?streams="

binanceStream :: [(String, StreamType)] -> ClientApp () -> IO ()
binanceStream [] = error "Please provide at least one symbol and stream type pair"
binanceStream ps = subscribeTo $ makeStreamName ps

------------------------------------------------------------
-- BINANCE USER API
--
type BinanceAccountApiTime =
  "time" :>
  Get '[ JSON] ServerTime

type BinanceAccountApiAllOrders =
  Header "X-MBX-APIKEY" Text :>
  "allOrders" :>
  QueryParam "symbol" Text :>
  QueryParam "orderId" Integer :>
  QueryParam "limit" Int :>
  QueryParam "recvWindow" Integer :>
  QueryParam "timestamp" Integer :>
  QueryParam "signature" Text :>
  Get '[ JSON] [OrderResponse]

-- type BinanceAccountApiMyTrades =
--   Header "X-MBX-APIKEY" Text :>
--   "myTrades" :>
--   QueryParam "symbol" Text :>
--   QueryParam "fromId" Integer :>
--   QueryParam "limit" Int :>
--   QueryParam "recvWindow" Integer :>
--   QueryParam "timestamp" Integer :>
--   QueryParam "signature" Text :>
--   Get '[ JSON] [Order]

type BinanceAccountApiTestOrder =
  Header "X-MBX-APIKEY" Text :>
  "order" :>
  "test" :>
--  ReqBody '[FormUrlEncoded] TestOrderParams :>
  QueryParam "quantity" Double :>
  QueryParam "symbol" Text :>
  QueryParam "type" OrderType :>
  QueryParam "side" Side :>
  QueryParam "timestamp" Integer :>
  QueryParam "signature" Text :>
  Post '[ JSON] Object

type BinanceAccountApi
     = "api" :> "v3" :>
        (    BinanceAccountApiTime
        :<|> BinanceAccountApiAllOrders
     --   :<|> BinanceAccountApiMyTrades
        :<|> BinanceAccountApiTestOrder
        )

binanceProxy :: Proxy BinanceAccountApi
binanceProxy = Proxy

getServerTime' :: ClientM ServerTime
allOrders' ::
       Maybe Text
    -> Maybe Text
    -> Maybe Integer
    -> Maybe Int
    -> Maybe Integer
    -> Maybe Integer
    -> Maybe Text
    -> ClientM [OrderResponse]
testOrder' ::
       Maybe Text
  --  -> TestOrderParams
    -> Maybe Double
    -> Maybe Text
    -> Maybe OrderType
    -> Maybe Side
    -> Maybe Integer
    -> Maybe Text
    -> ClientM Object
getServerTime' :<|> allOrders' :<|> testOrder' = client binanceProxy

getServerTime :: BinanceUserApi Integer
getServerTime = do
    url <- asks url
    man <- asks managr
    liftIO $ do
        Right (ServerTime time) <-
            runClientM getServerTime' $
            ClientEnv man url Nothing -- defaultMakeClientRequest
        return time

sign :: ByteString -> BinanceUserApi (Digest SHA256)
sign msg =
    asks privateKey >>= \secret ->
        return . hmacGetDigest . hmac secret $ msg

allOrders ::
       OrderParams
    -> BinanceUserApi (Either ClientError [OrderResponse])
allOrders params@OrderParams {..} = do
    url <- asks url
    man <- asks managr
    pub <- asks publicKey
    let msg = urlEncodeAsForm params
    sig <- sign $ toStrict msg
    liftIO $
        runClientM
            (allOrders'
                 (Just pub)
                 (Just opSymbol)
                 opOrderId
                 opLimit
                 opRecvWindow
                 (Just opTimestamp)
                 (Just ((pack . show) sig))) $
        ClientEnv man url Nothing -- defaultMakeClientRequest

testOrder ::
       TestOrderParams
    -> BinanceUserApi (Either ClientError Object)
testOrder params@TestOrderParams{..} = do
    url <- asks url
    man <- asks managr
    pub <- asks publicKey
    let msg = urlEncodeAsForm params
    sig <- sign $ toStrict msg
    liftIO $
        runClientM
            (testOrder'
                 (Just pub)
                 topQuantity
                 (Just topSymbol)
                 (Just topType)
                 (Just topSide)
                 (Just topTimestamp)
                 (Just ((pack . show) sig))) $
        ClientEnv man url Nothing -- defaultMakeClientRequest

