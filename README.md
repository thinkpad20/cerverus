## Cerverus: A Minimal Haskell Web Server

This doesn't do much right now except parse requests and URIs (the former of which is imported from Brian O'Sullivan's HTTP parser).

### Example usage:

After running `start RandomPort`, point your browser to `http://localhost:<port>/foo/bar?baz=qux`, where `<port>` is whatever random port it assigns. You should see something like this:

```
$ ghci

Prelude> :load Server.hs Parse.hs
[1 of 3] Compiling RequestResponse  ( RequestResponse.hs, interpreted )
[2 of 3] Compiling RFC2616          ( Parse.hs, interpreted )
[3 of 3] Compiling Main             ( Server.hs, interpreted )
Ok, modules loaded: RFC2616, Main, RequestResponse.
*Main> start RandomPort
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package filepath-1.3.0.1 ... linking ... done.
Loading package old-locale-1.0.0.5 ... linking ... done.
Loading package time-1.4.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package unix-2.6.0.1 ... linking ... done.
Loading package directory-1.2.0.1 ... linking ... done.
Loading package process-1.1.0.2 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package text-0.11.3.1 ... linking ... done.
Loading package attoparsec-0.10.4.0 ... linking ... done.
Loading package blaze-builder-0.3.1.1 ... linking ... done.
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package mtl-2.1.2 ... linking ... done.
Loading package parsec-3.1.3 ... linking ... done.
Loading package network-2.4.1.2 ... linking ... done.
Loading package primitive-0.5.0.1 ... linking ... done.
Loading package vector-0.10.0.1 ... linking ... done.
Loading package zlib-0.5.4.1 ... linking ... done.
Loading package zlib-bindings-0.1.1.3 ... linking ... done.
Loading package io-streams-1.1.2.0 ... linking ... done.
Loading package old-time-1.1.0.1 ... linking ... done.
Loading package HTTP-4000.2.8 ... linking ... done.
Loading package random-1.0.1.1 ... linking ... done.
Listening on port 60435
Connection received on [::1]:51293
Method: GET
Version: "1.1"
URI: Uri ("/foo/bar",fromList [("baz","qux")])
Headers: Header {headerName = "Host", headerValue = ["localhost:60435"]}
Header {headerName = "User-Agent", headerValue = ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:23.0) Gecko/20100101 Firefox/23.0"]}
Header {headerName = "Accept", headerValue = ["text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"]}
Header {headerName = "Accept-Language", headerValue = ["en-US,en;q=0.5"]}
Header {headerName = "Accept-Encoding", headerValue = ["gzip, deflate"]}
Header {headerName = "Connection", headerValue = ["keep-alive"]}
Connection received on [::1]:51294
Method: GET
Version: "1.1"
URI: Uri ("/favicon.ico",fromList [])
Headers: Header {headerName = "Host", headerValue = ["localhost:60435"]}
Header {headerName = "User-Agent", headerValue = ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:23.0) Gecko/20100101 Firefox/23.0"]}
Header {headerName = "Accept", headerValue = ["image/png,image/*;q=0.8,*/*;q=0.5"]}
Header {headerName = "Accept-Language", headerValue = ["en-US,en;q=0.5"]}
Header {headerName = "Accept-Encoding", headerValue = ["gzip, deflate"]}
Header {headerName = "Connection", headerValue = ["keep-alive"]}
Connection received on [::1]:51295
Method: GET
Version: "1.1"
URI: Uri ("/favicon.ico",fromList [])
Headers: Header {headerName = "Host", headerValue = ["localhost:60435"]}
Header {headerName = "User-Agent", headerValue = ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:23.0) Gecko/20100101 Firefox/23.0"]}
Header {headerName = "Accept", headerValue = ["text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"]}
Header {headerName = "Accept-Language", headerValue = ["en-US,en;q=0.5"]}
Header {headerName = "Accept-Encoding", headerValue = ["gzip, deflate"]}
Header {headerName = "Connection", headerValue = ["keep-alive"]}

```