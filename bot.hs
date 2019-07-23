import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Random
import Control.Monad

server = "irc.freenode.org"
port   = 6667
chan   = "#boths-testing"
nick   = "botik"

main = do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    write h "NICK" nick
    write h "USER" (nick++" 0 * :skirit's bot")
    write h "JOIN" chan
    kanal <- newTChanIO
    bot kanal h 
    listen kanal h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: TChan String -> Handle -> IO ()
listen kanal h = forever $ do
    t <- hGetLine h
    let s = init t
    if ping s then pong s else eval h (clean s)
    putStrLn s
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write h "PONG" (':' : drop 6 x)

eval :: Handle -> String -> IO ()
eval h    "!quit"                = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "!id " `isPrefixOf` x = privmsg h (drop 4 x)
eval _   _                       = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

bot kanal h = forkIO $ forever $ do
    threadDelay 10000000
    -- checkg gpio, set rgb diods, read last topit, update topic 
    -- TOPIC <channel> [<topic>]

