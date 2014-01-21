module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (ByteString)
import Data.String
import Data.Text (Text)
import Data.Maybe
import Network.Http.Client
import Network.HTTP.Conduit hiding (http)
import System.IO.Streams (InputStream, OutputStream, stdout)

import OpenSSL (withOpenSSL)
import OpenSSL.Session
import qualified System.IO.Streams as Streams

import qualified Network.Connection as Conn
import qualified Network.TLS as TLS
import qualified Network.TLS.Extra as TLS
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

ratingFromJSON :: ByteString -> Text
ratingFromJSON j = fromMaybe "invalid JSON!" (decode j >>= parseMaybe parser) 
  where parser = withObject "" (.: "rating")
  
conduitDefaultSettings :: IO Text
conduitDefaultSettings = ratingFromJSON <$> simpleHttp "https://www.howsmyssl.com/a/check"
  
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

ioStreams :: IO Text
ioStreams = withOpenSSL $ do
    ctx <- baselineContextSSL
    contextSetDefaultCiphers ctx
    q <- buildRequest $ do
      http GET "/check/a"
      setHostname "www.howsmyssl.com" 433
      setAccept "application/json"    
    c <- openConnectionSSL ctx "www.howsmyssl.com" 443
    sendRequest c q emptyBody
    response <- receiveResponse c concatHandler
    return $ ratingFromJSON $ B.fromChunks [response]

sslException :: SomeException -> IO Text
sslException _ = pure "SSL exception"

main :: IO ()
main = do
  putStr "http-conduit (using tls) with default settings is... "
  print =<< catch conduitDefaultSettings sslException
  putStr "http-conduit (using tls) with custom settings is... "
  print =<< catch conduitSecureSettings sslException
  putStr "http-streams (using HsOpenSSL) with default settings is... "
  print =<< catch ioStreams sslException