module Binance.Prelude
    ( module X
    ) where

import           Prelude as X
    ( String
    , Either (..)
    , IO
    , Maybe (..)
    , print
    , (>>)
    , pure
    , ($)
    , (>>=)
    )

import           Control.Concurrent       as X (forkIO)
import           Control.Monad            as X
    ( forever
    , unless
    )
import           Control.Monad.Reader     as X
    ( MonadIO
    , MonadReader
    , ReaderT
    , asks
    , runReaderT
    )
import           Control.Monad.Trans      as X (liftIO)
import           Crypto.Hash              as X (Digest)
import           Crypto.Hash.Algorithms   as X (SHA256)
import           Crypto.MAC.HMAC          as X
    ( hmac
    , hmacGetDigest
    )
import           Data.Aeson               as X
    ( FromJSON
    , Object
    , ToJSON
    , Value (String)
    , defaultOptions
    , encode
    , genericParseJSON
    , object
    , parseJSON
    , toJSON
    , withObject
    , (.:)
    , (.=)
    )
import           Data.Proxy               as X (Proxy (..))
import           Data.Text                as X
    ( Text
    , null
    , pack
    , unpack
    )
import           Data.Text.IO             as X
    ( getLine
    , putStrLn
    , readFile
    )
import           GHC.Generics             as X hiding (prec)
import           Network.Socket           as X
    ( withSocketsDo
    )
import           Network.WebSockets       as X
    ( ClientApp
    , receiveData
    , sendClose
    , sendTextData
    )
import           Servant.API              as X
    ( (:>)
    , FormUrlEncoded
    , Get
    , JSON
    , Post
    , QueryParam
    , ReqBody
    )
import           Servant.API.Alternative  as X
import           Servant.API.Header       as X (Header)
import           Servant.Client           as X
    ( BaseUrl (..)
    , Client
    , ClientEnv (..)
    , ClientM
    , Scheme (Https)
    , ClientError
    , client
    , runClientM
    )
import           Web.FormUrlEncoded       as X
    ( FormOptions (..)
    , FromForm
    , ToForm
    , genericToForm
    , toForm
    , urlEncodeAsForm
    )
import           Web.HttpApiData          as X
    ( FromHttpApiData
    , ToHttpApiData
    , parseUrlPiece
    , toEncodedUrlPiece
    , toQueryParam
    , toUrlPiece
    )
import           Web.Internal.HttpApiData as X
    ( unsafeToEncodedUrlPiece
    )
import           Wuss                     as X
    ( runSecureClient
    )
