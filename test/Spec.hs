{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS ( tlsManagerSettings)
import           Prelude             hiding (String)
-- import Data.Function ((&))
import Test.Hspec
import qualified Binance                 as H
import qualified Binance.Prelude         as P
import qualified Data.ByteString         as B (readFile)
import           Data.Aeson (decode)
import Debug.Trace

-- This is copy-pasted from the app, but really, there is no app
defaultConfig :: IO H.BinanceConfig
defaultConfig = do
    pubKey <- P.readFile "binance.pub"
    privKey <- B.readFile "binance.key"
    man <- newManager tlsManagerSettings
    pure H.BinanceConfig
      { H.url = P.BaseUrl P.Https "api.binance.com" 443 ""
      , H.managr = man
      , H.publicKey = pubKey
      , H.privateKey = privKey
      }

main :: IO ()
main = do
  config <- defaultConfig
  hspec $ do
    describe "All tests" $ do
      it "decodes" $
        decode "{\"e\":\"trade\",\"E\":1618586016940,\"s\":\"LINKUSDT\",\"t\":96946464,\"p\":\"40.38450000\",\"q\":\"161.37000000\",\"b\":1809872850,\"a\":1809872814,\"T\":1618586016910,\"m\":false,\"M\":true}" `shouldBe` Just (H.TradeResponse 1618586016910 40.38450000)
      it "Try time" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        t>1618037108339 `shouldBe` True
        -- This prints goofy output but I can't figure out how to write subtests using the t,
        -- except beforeAll which is different
      it "Try all orders" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params =
                H.OrderParams
                { H._symbol = "ADABTC"
                , H._orderId = Nothing
                , H._limit = Nothing
                , H._recvWindow = Nothing
                , H._timestamp = t
                }
        orders <- P.runReaderT (H.api (H.allOrders params)) config
        saneOrders orders `shouldBe` True
      it "Try test order" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params =
                H.TradeParams
                { H._symbol = "ADABTC"
                , H._side = H.BUY
                , H._type = H.MARKET
                , H._quantity = 1
                , H._timestamp = t
                , H._timeInForce = Nothing
                , H._price = Nothing
                , H._newClientOrderId = Nothing
                , H._stopPrice = Nothing
                , H._icebergQty = Nothing
                , H._newOrderRespType = Nothing
                , H._recvWindow = Nothing
                }
        trade <- P.runReaderT (H.api (H.testOrder params)) config
        saneTrade trade `shouldBe` True


saneOrders :: Either P.ClientError H.AllOrders -> Bool
saneOrders (Left _) = False
saneOrders (Right l) = length l > 1 && all (\p -> H._symbol (p::H.Order) == "ADABTC" ) l

saneTrade :: Either P.ClientError P.Object -> Bool
saneTrade (Left e) = trace (show e) False
saneTrade (Right o) = True -- trace (show o) o == []

