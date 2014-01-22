An Evaluation of Haskell SSL/TLS Libraries
==========================================

*tldr*: Neither of the available SSL/TLS packages for Haskell provides adequate security.

The Haskell ecosystem has two packages that provide SSL functionality for network connections: there is [tls][tls], which is written entirely in Haskell, and [HsOpenSSL][hsopenssl], which binds to the system OpenSSL library. The `tls` package is much more widely used than `HsOpenSSL`. Properly configured, `tls` (using the [http-conduit][httpconduit] library for HTTP requests) gets an "Improvable" rating from [howsmyssl.com][howsmyssl], whereas `HsOpenSSL` (using the [http-streams][httpstreams] package) gets a "Bad".

I have uploaded a cabal project that tests these libraries [here][evaluation]. 

### tls

The `tls` package is popular and reasonably stable: the `pipes` and `conduit` packages use it for streaming HTTP requests in constant time, and the [Yesod][yesod] web framework, which is built on `conduit`, uses it internally. The easiest way to start playing with it is to use the `http-conduit` package, which provides `simpleHttp`, a dead-simple interface to download the contents of a URL as a lazy `ByteString`:

    import Network.HTTP.Conduit
    import qualified Network.Connection as Conn
    import qualified Network.TLS as TLS
    import qualified Network.TLS.Extra as TLS
    
    checkSSL :: IO ByteString
    checkSSL = simpleHttp "https://www.howsmyssl.com/a/check"

However, the default settings (as specified by the `defaultParamsClient` function in the `tls` package) connects using TLS 1.0 by default, which gets it a "Bad" rating from How's My SSL. You must abandon the convenience API and build a new `ManagerSettings` record, passing it to the `withManagerSettings` function to override these defaults (the `pipes` package has a different but analogous API):

    
    
    settings :: ManagerSettings
    settings = mkManagerSettings (Conn.TLSSettings params) Nothing where
      params = TLS.defaultParamsClient 
        { TLS.pConnectVersion = TLS.TLS12
        , TLS.pAllowedVersions = [TLS.TLS12]
        -- uses the AES256_SHA256 and AES256_SHA1 cyphers
        , TLS.pCiphers = TLS.ciphersuite_strong
        }
    
    checkSSLSecure :: IO ByteString
    checkSSLSecure = do
      request <- httpLbs <$> parseUrl "https://www.howsmyssl.com/a/check"
      response <- withManagerSettings settings request
      return $ responseBody response

This yields us an "Improvable" rating, for two reasons: `tls` appears not to support session tickets (which, while suboptimal, is not a huge issue), and the tls-extra package (which provides all the cipher suites that `tls` uses) does not support any of the ECDHE cipher suites, which provide ephemeral key support. This is a glaring flaw. In addition, `tls` has **not** been vetted by any cryptographic experts. Though it is the most mature and usable library, please be aware of these factors before building any security-sensitive system with it.

### HsOpenSSL

The `HsOpenSSL` documentation recommends the use of the `tls` package despite acknowledging that `tls` has not been reviewed by a security professional, citing `HsOpenSSL`'s rudimentary error handling and older codebase as reasons to switch. Despite this, the `http-streams` package and the [Snap][snap] framework use it for SSL.

Attempting to connect to How's my SSL with the `http-streams` package led to the following response from the server:

    error:1407742E:SSL routines:SSL23_GET_SERVER_HELLO:tlsv1 alert protocol version

Jeff Hodges explains what this means:

    jmhodges: importantshock: that's it trying to handshake with an ssl v2 client hello
    jmhodges: with the "real" tls version inside it
    jmhodges: so that it can support sslv2 servers
    jmhodges: which is REAL BAD
    jmhodges: python 2.x just fixed that last week or so

It was unclear to me how to prevent `http-streams` from handshaking with an SSL2 client. The `HsOpenSSL` API is very rudimentary, to the point of unusability. In addition, even attempting to run the SSL example provided with `http-streams` caused an exception. I filed [a ticket][ticket] against HsOpenSSL. 

### Conclusion

Haskell may be a great language with a terrific ecosystem, but its lack of a solid SSL/TLS library means that it is probably not the best choice for secure HTTP programming. Going forward, the `tls` package would be improved by switching to TLS 1.2 by default and implementing a cipher suite that supports ephemeral keys.

[tls]: http://hackage.haskell.org/package/tls-1.1.5
[hsopenssl]: http://hackage.haskell.org/package/HsOpenSSL-0.10.4
[httpconduit]: http://hackage.haskell.org/package/http-conduit-2.0.0.3
[httpstreams]: https://hackage.haskell.org/package/http-streams
[howsmyssl]: https://www.howsmyssl.com
[evaluation]: https://github.com/patrickt/ssl-evaluation
[snap]: http://snapframework.com
[ticket]: https://github.com/afcowie/http-streams/issues/56
[yesod]: http://www.yesodweb.com
