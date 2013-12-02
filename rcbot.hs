module Main where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Network
import qualified Network.Socket as S
import Network.IRC
import System.IO
import Text.Printf

server = "irc.freenode.net"
port = 6667
nickname = "w8upd-rctest1"
channel = "#qsolog"

bot :: String -> IO Handle
bot nickname = withSocketsDo $ do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nickname
  write h "USER" (nickname ++ " 0 * :Recent Changes Bot")
  --mapM (write h "JOIN") channels
  write h "JOIN" channel
  return h

write :: Handle -> String -> String -> IO ()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

botPrivmsg :: Handle -> String -> IO ()
botPrivmsg h msg = write h "PRIVMSG" $ channel ++ " :" ++ msg

botListen :: Handle -> IO ()
botListen h = forever $ do
    s <- hGetLine h
    handleLine h $ decode s
    putStrLn s
  where
    forever a = a >> forever a

handleLine :: Handle -> Maybe Message -> IO ()
handleLine h Nothing = return ()
handleLine h (Just m) = reply $ msg_command m
  where
    params = msg_params m

    reply "PRIVMSG" = case response of
      Just s -> botPrivmsg h s >> return ()
      _ -> return ()
      where
        response =
          case params !! 1 of
            "!hello" -> Just "hey there"
            _ -> Nothing
    reply "PING" = write h "PONG " $ ":" ++ params !! 0
    reply _ = return ()

udpServer :: Handle -> IO ()
udpServer h = withSocketsDo $ do
    sock <- S.socket S.AF_INET S.Datagram 0
    S.bindSocket sock (S.SockAddrInet 2000 S.iNADDR_ANY)
    forever $ do
      (mesg, recv_count, client) <- S.recvFrom sock 1024
      send_count <- S.sendTo sock mesg client
      write h "PRIVMSG" $ channel ++ " :" ++ mesg
      putStrLn mesg

main :: IO ()
main = do
  h <- bot nickname
  forkIO $ do
    botListen h
  udpServer h