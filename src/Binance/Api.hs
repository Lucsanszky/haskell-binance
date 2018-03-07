{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Binance.Api
    ( module Binance.Type
    , app
    , binanceDepth
    , allOrders
    , getServerTime
    , testOrder
    , binanceProxy
    , BinanceAccountApi
    ) where

import           Binance.Prelude
import           Binance.Type
import           Data.ByteString      (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Char            (toLower)
import           Prelude              hiding
    ( getLine
    , null
    , putStrLn
    , readFile
    )

------------------------------------------------------------
-- BINANCE WEBSOCKET API
--
app :: ClientApp ()
app conn = do
    _ <-
        forkIO $
  -- Fork a thread that writes WS data to stdout
        forever $ do
            msg <- receiveData conn
            liftIO $ putStrLn msg
    loop
    sendClose conn ("Bye!" :: Text)
  where
    loop =
        getLine >>= \line ->
            unless (null line) $
            sendTextData conn line >> loop

binanceDepth :: Text -> ClientApp () -> IO ()
binanceDepth sym =
    withSocketsDo .
    runSecureClient "stream.binance.com" 9443 path
  where
    path = "/ws/" ++ unpack sym ++ "@depth"

------------------------------------------------------------
-- BINANCE USER API
--
type BinanceAccountApi
     = "api"
       :> (Header "X-MBX-APIKEY" Text
           :> "v3"
           :> "allOrders"
           :> QueryParam "symbol" Text
           :> QueryParam "orderId" Integer
           :> QueryParam "limit" Int
           :> QueryParam "recvWindow" Integer
           :> QueryParam "timestamp" Integer
           :> QueryParam "signature" Text
           :> Get '[ JSON] AllOrders
           :<|> "v1"
           :> "time"
           :> Get '[ JSON] ServerTime
           :<|> Header "X-MBX-APIKEY" Text
           :> "v3"
           :> "order"
           :> "test"
           :> ReqBody '[ FormUrlEncoded] TradeParams
           :> QueryParam "signature" Text
           :> Post '[ JSON] Object)

binanceProxy :: Proxy BinanceAccountApi
binanceProxy = Proxy

allOrders' ::
       Maybe Text
    -> Maybe Text
    -> Maybe Integer
    -> Maybe Int
    -> Maybe Integer
    -> Maybe Integer
    -> Maybe Text
    -> ClientM AllOrders
getServerTime' :: ClientM ServerTime
testOrder' ::
       Maybe Text
    -> TradeParams
    -> Maybe Text
    -> ClientM Object
allOrders' :<|> getServerTime' :<|> testOrder' =
    client binanceProxy

getServerTime :: BinanceUserApi Integer
getServerTime = do
    url <- asks url
    manager <- asks manager
    liftIO $ do
        Right (ServerTime time) <-
            runClientM getServerTime' $
            ClientEnv manager url
        return time

sign :: ByteString -> BinanceUserApi (Digest SHA256)
sign msg =
    asks privateKey >>= \secret ->
        return . hmacGetDigest . hmac secret $ msg

allOrders ::
       OrderParams
    -> BinanceUserApi (Either ServantError AllOrders)
allOrders params@OrderParams {..} = do
    url <- asks url
    manager <- asks manager
    pub <- asks publicKey
    let msg = urlEncodeAsForm params
    sig <- sign $ toStrict msg
    liftIO $
        runClientM
            (allOrders'
                 (Just pub)
                 (Just _symbol)
                 _orderId
                 _limit
                 _recvWindow
                 (Just _timestamp)
                 (Just ((pack . show) sig))) $
        ClientEnv manager url

testOrder ::
       TradeParams
    -> BinanceUserApi (Either ServantError Object)
testOrder params@TradeParams {..} = do
    url <- asks url
    manager <- asks manager
    pub <- asks publicKey
    let msg = urlEncodeAsForm params
    sig <- sign $ toStrict msg
    liftIO $
        runClientM
            (testOrder'
                 (Just pub)
                 params
                 (Just ((pack . show) sig))) $
        ClientEnv manager url
