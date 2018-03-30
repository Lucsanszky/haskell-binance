{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Binance                 as H
import qualified Binance.Prelude         as P
import qualified Data.ByteString         as B (readFile)
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS
    ( tlsManagerSettings
    )
import           Prelude
    ( Either (..)
    , IO
    , Maybe (..)
    , print
    )

main :: IO ()
main = do
    pubKey <-
        P.readFile "/Users/dlucsanszky/.ssh/binance-key.pub"
    privKey <-
        B.readFile "/Users/dlucsanszky/.ssh/binance-key"
    man <- newManager tlsManagerSettings
    -- Get the server time
    let config =
            H.BinanceConfig
            { H.url =
                  P.BaseUrl P.Https "api.binance.com" 443 ""
            , H.manager = man
            , H.publicKey = pubKey
            , H.privateKey = privKey
            }
    t <- P.runReaderT (H.api H.getServerTime) config
    -- Example to get all user orders for BNBBTC
    let params =
            H.OrderParams
            { H._symbol = "BNBBTC"
            , H._orderId = Nothing
            , H._limit = Nothing
            , H._recvWindow = Nothing
            , H._timestamp = t
            }
    orders <-
        P.runReaderT (H.api (H.allOrders params)) config
    case orders of
        Left err  -> print err
        Right res -> print res
    -- Example of test order creation
    let params2 =
            H.TradeParams
            { H._symbol = "LTCBTC"
            , H._side = H.BUY
            , H._type = H.LIMIT
            , H._timeInForce = Just "GTC"
            , H._quantity = 1
            , H._price = Just 0.1
            , H._newClientOrderId = Nothing
            , H._stopPrice = Nothing
            , H._icebergQty = Just 0
            , H._newOrderRespType = Just H.RESULT
            , H._recvWindow = Just 5000
            , H._timestamp = t
            }
    trade <-
        P.runReaderT (H.api (H.testOrder params2)) config
    case trade of
        Left err  -> print err
        Right res -> print res
    -- Example streams
    H.binanceStream
        [("BNBBTC", H.Depth), ("BNBETH", H.AggTrade)]
        H.app
