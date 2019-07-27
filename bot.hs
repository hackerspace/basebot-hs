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
    k <- newTChanIO
    bot k h 
    listen k h

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen :: TChan String -> Handle -> IO ()
listen k h = forever $ do
    t <- hGetLine h
    let s = init t
    eval h s
    putStrLn s
  where
    forever a = a >> forever a

eval :: Handle -> String -> IO ()
eval h x | "!quit" `isInfixOf`   x = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval h x | "PING :" `isPrefixOf` x = write h "PONG" (':' : drop 6 x)
eval _ _                           = return () -- ignore everything else

privmsg :: Handle -> String -> IO ()
privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)

bot k h = forkIO $ forever $ do
    threadDelay 10000000
    -- checkg gpio, set rgb diods, read last topit, update topic 
    -- TOPIC <channel> [<topic>]

