module Binance.Api
    ( module Binance.Type
    , app
    , allOrders
    , BinanceAccountApi
    , binanceStream
    , binanceProxy
    , getServerTime
    , testOrder
    ) where

import Binance.Prelude hiding (manager)
import Binance.Type
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Char (toLower)
import Data.List (intercalate)
import Prelude hiding (getLine, null, putStrLn, readFile)

------------------------------------------------------------
-- BINANCE WEBSOCKET API
--
app :: ClientApp ()
app conn = do
    _ <-
        forkIO $
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
  Get '[ JSON] AllOrders

type BinanceAccountApiTestOrder =
  Header "X-MBX-APIKEY" Text :>
  "order" :>
  "test" :>
  ReqBody '[ FormUrlEncoded] TradeParams :>
  QueryParam "signature" Text :>
  Post '[ JSON] Object

type BinanceAccountApi
     = "api" :> "v3" :>
        (    BinanceAccountApiTime
        :<|> BinanceAccountApiAllOrders
        :<|> BinanceAccountApiTestOrder
    --    :<|> BinanceAccountApiDepth
        )

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
getServerTime' :<|> allOrders' :<|> testOrder' = client binanceProxy

getServerTime :: BinanceUserApi Integer
getServerTime = do
    url <- asks url
    man <- asks manager
    liftIO $ do
        Right (ServerTime time) <-
            runClientM getServerTime' $
            ClientEnv man url Nothing defaultMakeClientRequest
        return time

sign :: ByteString -> BinanceUserApi (Digest SHA256)
sign msg =
    asks privateKey >>= \secret ->
        return . hmacGetDigest . hmac secret $ msg

allOrders ::
       OrderParams
    -> BinanceUserApi (Either ClientError AllOrders)
allOrders params@OrderParams {..} = do
    url <- asks url
    man <- asks manager
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
        ClientEnv man url Nothing defaultMakeClientRequest

testOrder ::
       TradeParams
    -> BinanceUserApi (Either ClientError Object)
testOrder params = do
    url <- asks url
    man <- asks manager
    pub <- asks publicKey
    let msg = urlEncodeAsForm params
    sig <- sign $ toStrict msg
    liftIO $
        runClientM
            (testOrder'
                 (Just pub)
                 params
                 (Just ((pack . show) sig))) $
        ClientEnv man url Nothing defaultMakeClientRequest
