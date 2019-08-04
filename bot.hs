import Network
import System.IO
import System.Timeout
import Text.Printf
import Data.List hiding (isSuffixOf)
import Data.Text (isSuffixOf, pack, stripEnd)
import System.Exit
import Control.Concurrent
import Control.Monad
import Control.Exception

server = "irc.freenode.org"
port   = 6667
chan   = "#base48"
nick   = "basebot-hs"
ofile  = "/sys/class/gpio/gpio2_pd2/value"
cfile  = "/sys/class/gpio/gpio1_pd0/value"
olfile = "/sys/class/gpio/gpio4_pd5/value"
clfile = "/sys/class/gpio/gpio5_pd6/value"
slfile = "/sys/class/gpio/gpio3_pd1/value"

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
    err ex = putStrLn ( "Error: " ++ show ex ) >> threadDelay (minutes 1)

write :: Handle -> String -> String -> IO ()
write h s t = do
    hPrintf h "%s %s\r\n" s t
    printf    "> %s %s\n" s t

listen k h = forever $ do
    m <- timeout (minutes 5) (hGetLine h)
    s <- case m of
        Nothing -> (hClose h >> error "Timeout")
        Just a -> return (init a)
    putStrLn s
    eval k h s

eval k h s | (pack ".beacon on")  `isSuffixOf` (stripEnd (pack s)) = writeFile slfile "1"
eval k h s | (pack ".beacon off") `isSuffixOf` (stripEnd (pack s)) = writeFile slfile "0"
eval k h s | "PING :" `isPrefixOf` s = write h "PONG" (':' : drop 6 s)
eval k h s | "TOPIC " `isPrefixOf` (command s) || "332 " `isPrefixOf` (command s) = do
    tryPutMVar k (content s)
    swapMVar k (content s)
    putStrLn ("kanal> " ++ (content s))
  where
    command = drop 1 . dropWhile (/= ' ')
    content = drop 1 . dropWhile (/= ':') . dropWhile (/= ' ')
eval _ _ _ = return () -- ignore everything else

checksw k h = forkIO $ forever $ do
    threadDelay (seconds 1)
    fo <- openFile ofile ReadMode
    fc <- openFile cfile ReadMode
    so <- hGetLine fo
    sc <- hGetLine fc
    hClose fo
    hClose fc
    sendled so sc
    m <- tryReadMVar k
    case m of
        Nothing -> return ()
        Just a -> do
            if so == "1" && sc == "0" && not ("base open " `isPrefixOf` a)
            then do 
                settopic h ("base open \\o/ " ++ (dropWhile (/= '|') a))
                threadDelay (seconds 5)
            else return ()
            if so == "0" && sc == "1" && not ("base closed " `isPrefixOf` a)
            then do
                settopic h ("base closed :( " ++ (dropWhile (/= '|') a))
                threadDelay (seconds 5)
            else return ()

settopic h s = catch (hPrintf h "%s%s\r\n" ("TOPIC " ++ chan ++ " :") s) err
  where
    err :: SomeException -> IO () 
    err ex = hClose h >> putStrLn ( "Error in checksw thread: " ++ show ex )
        
sendled so sc = do
    writeFile olfile so
    writeFile clfile sc
minutes m = m * seconds 1 * 60
seconds s = s * 1000000
