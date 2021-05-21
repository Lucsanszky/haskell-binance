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
            msg :: Maybe H.WT <- receiveData conn
            liftIO $ putStrLn ( msg & maybe "Unparsable" (pack . showForFile) ) --  >>= decodeStrict & maybe "" show )
    loop
    sendClose conn ("Bye!" :: Text)
  where
    loop =
        getLine >>= \line ->
            unless (Data.Text.null line) $
            sendTextData conn line >> loop

showForFile :: H.WT -> String
showForFile (H.WT s t p) = unpack s ++ " : " ++ show t ++ " " ++ show p

main :: IO ()
main = --pure () -- do
  H.binanceStream
    [ ("ADAUSDT", H.Trade)
    , ("LINKUSDT", H.Trade)
    , ("XRPUSDT", H.Trade)
    , ("XLMUSDT", H.Trade)
    , ("LINKUSDT", H.Trade)
    , ("BTCUSDT", H.Trade)
    , ("BCHUSDT", H.Trade)
    , ("BNBUSDT", H.Trade)
    , ("XMRUSDT", H.Trade)
    , ("ETHUSDT", H.Trade)
    , ("DOTUSDT", H.Trade)
    , ("LTCUSDT", H.Trade)
    ]
    app

