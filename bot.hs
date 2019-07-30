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
import Control.Exception

server = "irc.freenode.org"
port   = 6667
chan   = "#base48"
nick   = "basebot-hs"
ofile  = "/sys/class/gpio/gpio1_pd0/value"
cfile  = "/sys/class/gpio/gpio2_pd2/value"

main = forever $ catch mloop err
  where
    mloop = do
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        write h "NICK" nick
        write h "USER" (nick++" 0 * :open switch bot")
        write h "JOIN" chan
        k <- newEmptyMVar
        checksw k h
        listen k h
    err :: SomeException -> IO ()
    err ex = putStrLn ( "Error: " ++ show ex ) >> threadDelay (60 * 1000000) -- 60 s

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen k h = forever $ do
    t <- hGetLine h
    let s = init t
    putStrLn s
    eval k h s

--eval k h s | "!quit"  `isInfixOf`  s = write h "QUIT" ":Exiting" >> exitWith ExitSuccess
eval k h s | "PING :" `isPrefixOf` s = write h "PONG" (':' : drop 6 s)
eval k h s | "TOPIC " `isPrefixOf` (command s) || "332 " `isPrefixOf` (command s) = do
    tryPutMVar k (content s)
    swapMVar k (content s)
    putStrLn ("kanal> " ++ (content s))
  where
    command = drop 1 . dropWhile (/= ' ')
    content = drop 1 . dropWhile (/= ':') . drop 1
eval _ _ _ = return () -- ignore everything else

checksw k h = forkIO $ forever $ do
    threadDelay (1 * 1000000) -- 1 s
    fo <- openFile ofile ReadMode
    fc <- openFile cfile ReadMode
    so <- hGetLine fo
    sc <- hGetLine fc
    hClose fo
    hClose fc
    m <- tryReadMVar k
    case m of
        Nothing -> return ()
        Just a -> do
            if so == "1" && sc == "0" && not ("base open " `isPrefixOf` a)
            then settopic h ("base open \\o/ " ++ (dropWhile (/= '|') a))
            else return ()
            if so == "0" && sc == "1" && not ("base closed " `isPrefixOf` a)
            then settopic h ("base closed :( " ++ (dropWhile (/= '|') a))
            else return ()

settopic h s = hPrintf h "%s%s\r\n" ("TOPIC " ++ chan ++ " :") s
--privmsg h s = write h "PRIVMSG" (chan ++ " :" ++ s)
