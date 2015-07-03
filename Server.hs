module Main where

import Util

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Function
import           Data.Monoid
import           System.Environment
import           System.Exit
import           System.IO
import           System.Posix.IO
import           System.Process           hiding (createPipe)
import           System.ZMQ4.Monadic
import           Text.Printf

main :: IO ()
main = getArgs >>= \case
    [] -> do
        progName <- getProgName
        hPutStrLn stderr $ printf "Usage: %s <prog> [<arg> ...]" progName
        exitFailure

    (x:xs) -> do
        printf "Spawning '%s'...\n" (unwords (x:xs))

        (stdinReadFd,  stdinWriteFd)  <- createPipe
        (stdoutReadFd, stdoutWriteFd) <- createPipe
        (stderrReadFd, stderrWriteFd) <- createPipe

        stdinReadH   <- fdToHandle stdinReadFd
        stdinWriteH  <- fdToHandle stdinWriteFd
        stdoutReadH  <- fdToHandle stdoutReadFd
        stdoutWriteH <- fdToHandle stdoutWriteFd
        stderrReadH  <- fdToHandle stderrReadFd
        stderrWriteH <- fdToHandle stderrWriteFd

        hSetBuffering stdinReadH   LineBuffering
        hSetBuffering stdinWriteH  LineBuffering
        hSetBuffering stdoutReadH  NoBuffering
        hSetBuffering stdoutWriteH NoBuffering
        hSetBuffering stderrReadH  NoBuffering
        hSetBuffering stderrWriteH NoBuffering

        void $ createProcess (proc x xs)
                   { std_in  = UseHandle stdinReadH
                   , std_out = UseHandle stdoutWriteH
                   , std_err = UseHandle stdoutWriteH
                   }

        runZMQ $ do
            subscriber <- socket Sub
            bind subscriber "tcp://127.0.0.1:9000"
            subscribe subscriber ""

            publisher <- socket Pub
            bind publisher "tcp://127.0.0.1:9001"

            liftIO . putStrLn $ "Press enter when client(s) are ready."
            void $ liftIO getLine

            let in_callback _ = do
                    req@(Request _ msg) <- parseRequest =<< receiveMulti subscriber
                    liftIO $ BS.hPutStrLn stdinWriteH msg
                    sendMulti publisher (serializeMessage req)

                out_callback _ =
                    hGetAvailable stdoutReadH >>=
                      sendMulti publisher . serializeMessage . Response Stdout

                err_callback _ =
                    hGetAvailable stderrReadH >>=
                      sendMulti publisher . serializeMessage . Response Stderr

            fix $ \loop -> do
                evts <- concat <$>
                    poll (-1) [ Sock subscriber   [In] (Just in_callback)
                              , File stdoutReadFd [In] (Just out_callback)
                              , File stderrReadFd [In] (Just err_callback)
                              ]

                unless (Err `elem` evts)
                    loop

        hClose stdinReadH
        hClose stdinWriteH
        hClose stdoutReadH
        hClose stdoutWriteH
        hClose stderrReadH
        hClose stderrWriteH

-- | Get all available bytes on a socket.
hGetAvailable :: MonadIO m => Handle -> m ByteString
hGetAvailable h = do
    bytes <- liftIO $ BS.hGetNonBlocking h blockSize
    if BS.length bytes == blockSize
        then (bytes <>) <$> hGetAvailable h
        else return bytes
  where
    blockSize = 2048
