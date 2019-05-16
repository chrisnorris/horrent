#!/usr/bin/env stack
{- stack --resolver lts-13.21 --install-ghc
exec ghci
      --package bencode
      --package lens
      --package cryptohash
      --package http-client
      --package http-conduit
      --package yaml
      --package byteable
      --package iproute
-}
{-# LANGUAGE ViewPatterns, RecordWildCards, FlexibleContexts, OverloadedStrings, TemplateHaskell, DeriveGeneric, PackageImports #-}

module Main 
       (
         runScrape,
         main
       ) where

{-
Full specification :  http://www.bittorrent.org/beps/bep_0003.html
-}

import           Control.Lens
import           Control.Monad                  ( replicateM_ )
import           Data.Binary                    ( Binary(..)
                                                , encode
                                                , decodeOrFail
                                                , getWord8
                                                )
import           Data.Binary.Get                ( Get(..)
                                                , ByteOffset
                                                , runGet
                                                , getWord32le
                                                , getWord16be
                                                , getByteString
                                                , skip
                                                )
import           Data.Binary.Put                ( putWord8, putByteString)
import           Data.Byteable                  ( toBytes )
import           Data.IP                        ( toIPv4 )
import           Data.Maybe (fromJust)
import           Data.Monoid                    ( mappend , (<>))
import           Data.Word                      ( Word16 , Word8)
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client            ( path )
import           Network.HTTP.Simple            ( setRequestMethod
                                                , setRequestQueryString
                                                , getResponseBody
                                                , parseRequest
                                                , httpLBS
                                                )
import           Network.Socket                 ( addrAddress
                                                , addrSocketType
                                                , addrFamily
                                                , addrProtocol
                                                , connect
                                                , close
                                                , defaultHints
                                                , PortNumber
                                                , socket
                                                , SocketType(Stream)
                                                , SockAddr(SockAddrInet)
                                                , withSocketsDo
                                                , getAddrInfo
                                                )
import           Network.Socket.ByteString      ( sendAll, recv )

import qualified Control.Exception as E
import qualified Data.Map                      as Map
import qualified Data.BEncode                  as Bencode
import qualified "cryptohash" Crypto.Hash      as CH
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as C
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as SL8
import qualified Data.Maybe                    as DM

makePrisms ''Bencode.BEncode

protocolString :: B.ByteString
protocolString = "BitTorrent protocol"

mkPeerId = "ABCDEFGHIJKLMNOPQRST"
torrentName = "kubuntu-18.04.2-desktop-i386.iso.torrent" --"archlinux-2018.06.01-x86_64.iso.torrent"

type PeerId = String
type InfoHash = CH.Digest CH.SHA1
type TorrentData = BL.ByteString
type TorrentLength = Integer

data MetaInfo = MetaInfo
  { info     :: SingleFileInfoDictionary
  , infoHash :: InfoHash
  , announce :: TorrentData
  } deriving Show

data SingleFileInfoDictionary = SingleFileInfoDictionary
  { lengths     :: TorrentLength
    --, md5sum  :: Maybe ByteString
  , name        :: TorrentData
  , pieceLength :: TorrentLength
  , pieces      :: TorrentData
  } deriving Show

data BHandshake = BHandshake {
   handshakeInfoHash :: B.ByteString
 , handshakePeerId   :: B.ByteString
 } deriving(Show, Eq, Generic)


instance Binary BHandshake where
  put (BHandshake infoHash peerId) = do
    putWord8 (fromIntegral $ B.length protocolString)
    putByteString protocolString
    replicateM_ 8 (putWord8 0)
    putByteString infoHash
    putByteString peerId

  get = do
    protoSize <- get :: Get Word8
    replicateM_ (fromIntegral protoSize) getWord8
    skip 8
    infoHash <- getByteString 20
    skip 20
    peerId <- getByteString 20
    return $ BHandshake peerId infoHash

main = getMetaInfo >>= getPeers
      -- for all peers, setup handshake and then communicate on a separate thread
      -- threads communicate with main state which uses the tracker to orchestrate
      -- piece retrieval

runScrape = getMetaInfo >>= scrape

getMetaInfo = makeMetaInfo . decode <$> input
 where
  input  = BL.readFile $ "/Users/RBird/Downloads/" <> torrentName
  decode = fromJust . Bencode.bRead
  makeMetaInfo parsedTorrent =
    let infoDictionary = fromJust $ parsedTorrent ^. _BDict ^. at "info"
        singleFileInfoDictionary = SingleFileInfoDictionary
          (key "length" _BInt)
          --(fromMaybe ((infodictionary ^. at "md5sum")^? _BString) Nothing)
          (key "name" _BString)
          (key "piece length" _BInt)
          (key "pieces" _BString)
           where
            key name prism =
              fromJust (fromJust (infoDictionary ^. _BDict ^. at name) ^? prism)
    in MetaInfo
          singleFileInfoDictionary
          (CH.hash . BL.toStrict $ Bencode.bPack infoDictionary)
          (fromJust (parsedTorrent ^. _BDict ^. at "announce") ^. _BString)

getPeers meta@MetaInfo {..} = do
  request' <- parseRequest $ SL8.unpack announce
  let request = setRequestMethod "GET" $ setRequestQueryString
        [ ("info_hash" , Just $ toBytes infoHash)
        , ("peer_id"   , Just mkPeerId)
        , ("port"      , Just $ C.pack "6881")
        , ("uploaded"  , Just "0")
        , ("downloaded", Just "0")
        , ("left"      , Just $ (C.pack . show) $ lengths info)
        , ("event"     , Just $ C.pack "started")
        , ( "numwant"  , Just "100")
        -- ,("no_peer_id"   , Just "1")
        , ("compact", Just "0")
        ]
        request'

  responseB <- getResponseBody <$> httpLBS request
  let peers =
        chunksOf 6
          .  fromJust
          $  fromJust (bendecodedBody ^. _BDict ^. at "peers")
          ^? _BString
       where
        chunksOf _ (BL.uncons -> Nothing) = []
        chunksOf n l                      = first : chunksOf n rest
          where (first, rest) = BL.splitAt n l
        bendecodedBody = DM.fromJust (Bencode.bRead responseB)
  let peersToIPAndPort = toIPPortTuple <$> peers
        where
         toIPPortTuple x = SockAddrInet (fromIntegral (runGet (skip 4 >> getWord16be) x) :: PortNumber)
                           $ runGet getWord32le (SL8.take 4 x)
  return peersToIPAndPort

scrape meta@MetaInfo {..} = do
  request <- parseRequest $ SL8.unpack announce
  let requestScrape = setRequestMethod "GET" $ setRequestQueryString [] $ request {path = "/scrape"}
  responseS <- Bencode.bRead . getResponseBody <$> httpLBS requestScrape
  let scrapeResponse = fromJust responseS
  let parsedFilenames = over traverse
                             (view _BString . fromJust)
                             bdecodedFilenames
       where
        bdecodedFilenames =
          over traverse (view (at "name") . view _BDict)
            $  Map.elems
            $  fromJust (Map.lookup "files" $ scrapeResponse ^. _BDict)
            ^. _BDict

  mapMOf_ traverse SL8.putStrLn parsedFilenames
  return (scrapeResponse, tup scrapeResponse ^. _2 . _BString)
  where
   tup x =
     Map.elemAt 3
       $  head (Map.elems (head (Map.elems $ x ^. _BDict) ^. _BDict))
       ^. _BDict


handshakePeer = do
  metaInfo <- getMetaInfo
  let handShake = BHandshake (toBytes $ infoHash metaInfo) mkPeerId
  addr <- resolve "99.198.199.144" "51410"
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  putStrLn "Connecting.."

  connect sock $ addrAddress addr
  -- h <- socketToHandle sock ReadWriteMode
  C.putStrLn $ BL.toStrict $ encode handShake
  sendAll sock (BL.toStrict $ encode handShake) --"GET /httpbin/ HTTP/1.1\r\nHost: \r\nConnection: close\r\n\r\n"
  input <- recv sock 68
  C.putStrLn input
  let str = BL.fromStrict input
  
  let res = decodeOrFail str :: (Either (BL.ByteString, ByteOffset, String) (BL.ByteString, ByteOffset, BHandshake))
  return res
  -- case res of
  --     Left (remainder, offset, err ) -> putStrLn  $ err ++ " at : " ++ show offset ++ ", LEAVES: " ++ BC.unpack remainder  -- the handshake is wrong/unsupported
  --     Right (_, _, handshake) -> print handshake
  
  --hGetContents h >>= putStrLn

  where
      resolve host port = do
                let hints = defaultHints { addrSocketType = Stream }
                addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
                return addr

testHandshake = withSocketsDo $ do
      addr <- resolve "52.20.16.20" "30001"
      E.bracket (open addr) close talk
    where
      resolve host port = do
          let hints = defaultHints { addrSocketType = Stream }
          addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
          return addr
      open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          connect sock $ addrAddress addr
          return sock
      talk sock = do
          sendAll sock "get back the TCP info"
          msg <- recv sock 1024
          putStr "Received: "
          C.putStrLn msg


