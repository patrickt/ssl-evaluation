

module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.String
import Data.Text (Text)
import Data.Maybe
import Network.HTTP.Conduit

import qualified Network.Connection as Conn
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified OpenSSL as SSL
import qualified OpenSSL.Session as SSL
import qualified System.IO.Streams.SSL as SSLStreams

ratingFromJSON :: ByteString -> Text
ratingFromJSON j = fromMaybe "invalid JSON!" (decode j >>= parseMaybe parser) 
  where parser = withObject "" (.: "rating")
  
settings :: ManagerSettings
settings = mkManagerSettings (Conn.TLSSettings params) Nothing where
  params = TLS.defaultParamsClient 
    { TLS.pConnectVersion = TLS.TLS12
    , TLS.pAllowedVersions = [TLS.TLS12]
    , TLS.pCiphers = TLS.ciphersuite_strong
    }
  
conduitSecureSettings :: IO Text
conduitSecureSettings = do
  request <- httpLbs <$> parseUrl "https://www.howsmyssl.com/a/check"
  response <- withManagerSettings settings request
  return $ ratingFromJSON $ responseBody response

conduitDefaultSettings :: IO Text
conduitDefaultSettings = ratingFromJSON <$> simpleHttp "https://www.howsmyssl.com/a/check"

main :: IO ()
main = do
  putStr "Http-conduit with default settings is... "
  print =<< conduitDefaultSettings
  putStr "Http-conduit with custom settings is... "
  print =<< conduitSecureSettings