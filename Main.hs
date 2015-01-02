module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import Network.Bruce.Socket
import Options.Applicative

import qualified Network.Socket as N

main :: IO ()
main = do
    o  <- parseOptions
    is <- inSock o
    os <- outSock o
    bind is
    listen is 10
    connect os
    forever $ accept is >>= relay os
  where
    inSock  o = streamSocket (optHost o) (optPort o)
    outSock o = datagramSocket (optDest o) defaultBufferSize

relay :: Socket -> Socket -> IO ()
relay os is = void $ fwd `forkFinally` const (close is)
  where
    fwd = forever $ recv is >>= send os

-- Options

data Opts = Opts
    { optHost :: N.HostName
    , optPort :: N.PortNumber
    , optDest :: FilePath
    } deriving (Eq, Show)

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Bruce TCP Relay" <> fullDesc

    optsParser :: Parser Opts
    optsParser = Opts
        <$> (strOption $
                long "host"
                <> metavar "HOSTNAME"
                <> help "Hostname or address to bind to")
        <*> (fmap N.PortNum . option auto $
                long "port"
                <> short 'p'
                <> metavar "PORT"
                <> help "Port to listen on")
        <*> (strOption $
                long "dest"
                <> short 'd'
                <> metavar "FILE"
                <> help "Path to the Bruce UNIX domain datagram socket")
