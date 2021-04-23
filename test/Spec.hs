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

traceme :: Show a => String -> a -> a
traceme s a = a `seq` traceShowPreF s id a

traceShowPreF :: (Show b) => String -> (a -> b) -> a -> a
traceShowPreF prefix f a = trace (prefix ++ show (f a)) a


-- This is copy-pasted from the app, but really, there is no app
defaultConfig :: IO H.BinanceConfig
defaultConfig = do
    pubKey <- P.readFile "../../binance.pub"
    privKey <- B.readFile "../../binance.key"
    man <- newManager tlsManagerSettings
    pure H.BinanceConfig
      { H.url = P.BaseUrl P.Https "api.binance.com" 443 ""
      , H.managr = man
      , H.publicKey = pubKey
      , H.privateKey = privKey
      }

aOrderParams :: Integer -> H.OrderParams
aOrderParams t = H.OrderParams
                { H._symbol = "ADAUSDT"
                , H._orderId = Nothing
                , H._limit = Nothing
                , H._recvWindow = Nothing
                , H._timestamp = t
                }

aTradeParams :: Integer -> H.TradeParams
aTradeParams t = H.TradeParams
                { H._symbol = "ADAUSDT"
                , H._side = H.BUY
                , H._type = H.MARKET
                , H._quantity = Just 10
               -- , H._quoteOrderQty = Nothing
               -- , H._timeInForce = Nothing
               -- , H._price = Nothing
               -- , H._newClientOrderId = Nothing
               -- , H._stopPrice = Nothing
               -- , H._icebergQty = Nothing
               -- , H._newOrderRespType = Nothing
               -- , H._recvWindow = Nothing
                , H._timestamp = t
                }

main :: IO ()
main = do
  config <- defaultConfig
  hspec $ do
    describe "All tests" $ do
      it "Decodes" $
        decode "{\"e\":\"trade\",\"E\":1618586016940,\"s\":\"LINKUSDT\",\"t\":96946464,\"p\":\"40.38450000\",\"q\":\"161.37000000\",\"b\":1809872850,\"a\":1809872814,\"T\":1618586016910,\"m\":false,\"M\":true}" 
          `shouldBe` Just (H.Deal "LINKUSDT" 1618586016910 40.38450000)
      it "To and from form OrderParams" $
         P.urlEncodeAsForm (aOrderParams 1) `shouldBe` "symbol=ADAUSDT&timestamp=1"
      it "To and from form TradeParams" $
         P.urlEncodeAsForm (aTradeParams 1) `shouldBe` "quantity=10.0&symbol=ADAUSDT&type=MARKET&side=BUY&timestamp=1"
      --it "To and from form 2" $
      --   P.toEncodedUrlPiece (aTradeParams 1) `shouldBe` "symbol=ADAGBP&quoteOrderQty=1.0&type=MARKET&side=BUY&timestamp=1"
      it "Try time" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        t>1618037108339 `shouldBe` True
        -- This prints goofy output but I can't figure out how to write subtests using the t,
        -- except beforeAll which is different
      it "Try all orders" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params =
                H.OrderParams
                { H._symbol = "ADAUSDT"
                , H._orderId = Nothing
                , H._limit = Nothing
                , H._recvWindow = Nothing
                , H._timestamp = t
                }
        orders <- P.runReaderT (H.api (H.allOrders params)) config
        saneOrders orders `shouldBe` True
      it "Try test order" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        let params = aTradeParams t
        trade <- P.runReaderT (H.api (H.testOrder params)) config
        saneTrade trade `shouldBe` True


saneOrders :: Either P.ClientError H.AllOrders -> Bool
saneOrders (Left e) = trace (show e) False
saneOrders (Right l) = length l > 1 && all (\p -> H._symbol (p::H.Order) == "ADAUSDT" ) l

saneTrade :: Either P.ClientError P.Object -> Bool
saneTrade (Left e) = trace (show e) False
saneTrade (Right o) = True -- trace (show o) o == []

-- FailureResponse (Request {requestPath = (BaseUrl {baseUrlScheme = Https, baseUrlHost = "api.binance.com", baseUrlPort = 443, baseUrlPath = ""},"/api/v3/order/test"), requestQueryString = fromList [("signature",Just "74d0513cb530c13ee200eb0f282f0eb94327137a1cf42f59fc1f45aa632f4118")], requestBody = Just ((),application/x-www-form-urlencoded), requestAccept =fromList [application/json;charset=utf-8,application/json], requestHeaders = fromList [("X-MBX-APIKEY","lLh49BpSIuoJeXsE2jepXyBZU4b9dOyQrGd9edQ2eaGm2UM2MC8gykDy1hxSAPtT")]), requestHttpVersion = HTTP/1.1, requestMethod = "POST"} 
-- (Response {responseStatusCode = Status {statusCode = 400, statusMessage = "Bad Request"}, responseHeaders = fromList [("Content-Type","application/json;charset=UTF-8"),("Content-Length","51"),("Connection","keep-alive"),("Date","Sat, 17 Apr 2021 11|32| 23 GMT"),("Server","nginx"),("x-mbx-uuid","e4ba3911-c1ea-40be-befc-ee2894532189"),("x-mbx-used-weight","9"),("x-mbx-used-weight-1m","9"),("Strict-Transport-Security","max-age=3153600

