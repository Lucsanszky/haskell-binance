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

-- This is copy-pasted from the app, but really, there is no app
defaultConfig :: IO H.BinanceConfig
defaultConfig = do
    pubKey <- P.readFile "binance.pub"
    privKey <- B.readFile "binance.key"
    man <- newManager tlsManagerSettings
    pure H.BinanceConfig
      { H.url = P.BaseUrl P.Https "api.binance.com" 443 ""
      , H.manager = man
      , H.publicKey = pubKey
      , H.privateKey = privKey
      }

main :: IO ()
main = do
  config <- defaultConfig
  hspec $ do
    describe "All tests" $ do
      it "Try time and orders" $ do
        t <- P.runReaderT (H.api H.getServerTime) config
        t>1618037108339 `shouldBe` True
        let params =
                H.OrderParams
                { H._symbol = "ADABTC"
                , H._orderId = Nothing
                , H._limit = Nothing
                , H._recvWindow = Nothing
                , H._timestamp = t
                }
        orders <- P.runReaderT (H.api (H.allOrders params)) config
        sane orders `shouldBe` True
          where
          sane :: Either P.ClientError H.AllOrders -> Bool
          sane (Left _) = False
          sane (Right l) = length l > 1 && all (\p -> H._symbol (p::H.Order) == "ADABTC" ) l





