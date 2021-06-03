{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS ( tlsManagerSettings)
import           Prelude             -- hiding (String)
-- import Data.Function ((&))
import Test.Hspec
import qualified Binance                 as H
import qualified Binance.Prelude         as P
import qualified Data.ByteString         as B (readFile)
import qualified Data.Map.Strict         as HM
import           Data.Aeson (decode)
import Debug.Trace


-- This is copy-pasted from the app, but really, there is no app
defaultConfig :: IO H.BinanceConfig
defaultConfig = do
    pubKey <- P.readFile "../binance.ad.pub"
    privKey <- B.readFile "../binance.ad.key"
    man <- newManager tlsManagerSettings
    pure H.BinanceConfig
      { H.url = P.BaseUrl P.Https "api.binance.com" 443 ""
      , H.managr = man
      , H.publicKey = pubKey
      , H.privateKey = privKey
      }

aAllOrdersRequest :: Integer -> H.AllOrdersRequest
aAllOrdersRequest t = H.AllOrdersRequest
                { H.aopSymbol = "ADAUSDT"
                , H.aopOrderId = Nothing
                , H.aopLimit = Nothing
                , H.aopRecvWindow = Nothing
                , H.aopTimestamp = t
                }

aMyTradesRequest :: Integer -> H.MyTradesRequest
aMyTradesRequest t = H.MyTradesRequest
                { H.mtpSymbol = "ADAUSDT"
                , H.mtpFromId = Nothing
                , H.mtpLimit = Nothing
                , H.mtpRecvWindow = Nothing
                , H.mtpTimestamp = t
                }

aTestOrderRequest :: Integer -> H.TestOrderRequest
aTestOrderRequest t = H.TestOrderRequest
                { H.topSymbol = "ADAUSDT"
                , H.topSide = H.BUY
                , H.topType = H.MARKET
                , H.topQuantity = Just 10
               -- , H.topQuoteOrderQty = Nothing
               -- , H.topTimeInForce = Nothing
               -- , H.topPrice = Nothing
               -- , H.topNewClientOrderId = Nothing
               -- , H.topStopPrice = Nothing
               -- , H.topIcebergQty = Nothing
               -- , H.topNewOrderRespType = Nothing
               -- , H.topRecvWindow = Nothing
                , H.topTimestamp = t
                }

aAccountRequest :: Integer -> H.AccountRequest
aAccountRequest t = H.AccountRequest
                { H.apRecvWindow = Nothing
                , H.apTimestamp = t
                }

main :: IO ()
main = do
  config <- defaultConfig
  hspec $ do
    describe "All tests" $ do
      it "Decodes" $
        decode "{\"e\":\"trade\",\"E\":1618586016940,\"s\":\"LINKUSDT\",\"t\":96946464,\"p\":\"40.38450000\",\"q\":\"161.37000000\",\"b\":1809872850,\"a\":1809872814,\"T\":1618586016910,\"m\":false,\"M\":true}" 
          `shouldBe` Just (H.Deal "LINKUSDT" 1618586016910 40.38450000)
      it "To and from form AllOrdersRequest" $
         P.urlEncodeAsForm (aAllOrdersRequest 1) `shouldBe` "symbol=ADAUSDT&timestamp=1"
      it "To and from form TestOrderRequest" $
         P.urlEncodeAsForm (aTestOrderRequest 1) `shouldBe` "quantity=10.0&symbol=ADAUSDT&type=MARKET&side=BUY&timestamp=1"
      it "Try time" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        t>1618037108339 `shouldBe` True
        -- This prints goofy output but I can't figure out how to write subtests using the t,
        -- except beforeAll which is different
      it "Try all orders" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params = aAllOrdersRequest t
        orders <- P.runReaderT (H.api (H.allOrders params)) config
        saneOrders orders `shouldBe` True
      it "Try my trades" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params = aMyTradesRequest t
        mytrades <- P.runReaderT (H.api (H.myTrades params)) config
        saneMyTrades mytrades `shouldBe` True
      it "Try account" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params = aAccountRequest t
        acc <- P.runReaderT (H.api (H.account params)) config
        saneAcc acc `shouldBe` True
      it "Try test order" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params = aTestOrderRequest t
        trade <- P.runReaderT (H.api (H.testOrder params)) config
        saneTrade trade `shouldBe` True


saneOrders :: Either P.ClientError [H.AllOrdersResponseLine] -> Bool
saneOrders (Left e) = trace (show e) False
saneOrders (Right l) = length l > 1 && all (\p -> H.aorSymbol (p::H.AllOrdersResponseLine) == "ADAUSDT" ) l

saneMyTrades :: Either P.ClientError [H.MyTradesResponseLine] -> Bool
saneMyTrades (Left e) = trace (show e) False
saneMyTrades (Right l) = length l > 1 && all (\p -> H.mtrSymbol (p::H.MyTradesResponseLine) == "ADAUSDT" ) l

saneAcc :: Either P.ClientError H.AccountResponseStupid -> Bool
saneAcc (Left e) = trace (show e) False
saneAcc (Right o) = True -- trace (show o) o == []

saneTrade :: Either P.ClientError P.Object -> Bool
saneTrade (Left e) = trace (show e) False
saneTrade (Right o) = True -- trace (show o) o == []

