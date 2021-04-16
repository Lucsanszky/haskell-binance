{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import           Control.Concurrent (forkIO)
import           Control.Monad (forever, unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (decodeStrict)
import           Data.Function ((&))
import           Data.Text
import           Data.Text.IO (readFile, putStrLn, getLine)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS ( tlsManagerSettings)
import           Network.WebSockets
import           Prelude hiding (readFile, getLine, putStrLn)
import           Servant.Client
import qualified Binance                 as H
import qualified Data.ByteString         as B (ByteString, readFile, unpack)
import qualified Data.ByteString.Lazy as LB

defaultConfig :: IO H.BinanceConfig
defaultConfig = do
    pubKey <- readFile "binance.pub"
    privKey <- B.readFile "binance.key"
    man <- newManager tlsManagerSettings
    pure H.BinanceConfig
      { H.url = BaseUrl Https "api.binance.com" 443 ""
      , H.managr = man
      , H.publicKey = pubKey
      , H.privateKey = privKey
      }


app :: ClientApp ()
app conn = do
    _ <-
        forkIO $
        forever $ do
            msg :: Maybe H.TradeResponse <- receiveData conn
            liftIO $ putStrLn ( msg & maybe "Foo" (pack . show) ) --  >>= decodeStrict & maybe "" show )
    loop
    sendClose conn ("Bye!" :: Text)
  where
    loop =
        getLine >>= \line ->
            unless (Data.Text.null line) $
            sendTextData conn line >> loop

main :: IO ()
main = --pure () -- do
--    config <- defaultConfig
    -- Example to get all user orders for BNBBTC
--    case orders of
--        Left err  -> print ("Err: "::String) >> print err
--        Right res -> print ("Res: "::String) >> print res
--    -- Example of test order creation
--    let params2 =
--            H.TradeParams
--            { H._symbol = "BTCLTC"
--            , H._side = H.BUY
--            , H._type = H.LIMIT
--            , H._timeInForce = Just "GTC"
--            , H._quantity = 1
--            , H._price = Just 0.1
--            , H._newClientOrderId = Nothing
--            , H._stopPrice = Nothing
--            , H._icebergQty = Just 0
--            , H._newOrderRespType = Just H.RESULT
--            , H._recvWindow = Just 5000
--            , H._timestamp = t
--            }
--    trade <-
--        P.runReaderT (H.api (H.testOrder params2)) config
--    case trade of
--        Left err  -> print ("Err: "::String) >> print err
--        Right res -> print ("Res: "::String) >> print res
--    -- Example streams
  H.binanceStream
    --[("BNBBTC", H.Depth), ("BNBETH", H.AggTrade)]
    [("LINKUSDT", H.Trade)]
    app

