module Main where

import Util

import           Control.Exception      (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Managed
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Function
import           Data.Monoid
import           Options.Applicative
import           System.IO
import           System.Posix.IO
import           System.Posix.Types     (Fd)
import           System.Process         hiding (createPipe)
import           System.ZMQ4.Monadic
import           Text.Printf

data Args = Args
    { argEndpoint :: String
    , argPort     :: Int
    , argCommand  :: [String]
    }

main :: IO ()
main = execParser opts >>= main'
  where
    opts :: ParserInfo Args
    opts = info (helper <*> parseArgs) $ mconcat
        [ fullDesc
        , progDesc ("Start a server running <PROG ARG...> on <ENDPOINT:PORT> "
                    ++ "(input) and <ENDPOINT+PORT+1> (output)")
        , header "Coop server v0.1.0.0"
        ]

    parseArgs :: Parser Args
    parseArgs = Args
        <$> strOption (mconcat
            [ short 'e'
            , long "endpoint"
            , metavar "ENDPOINT"
            , value "127.0.0.1"
            , showDefault
            , help "Endpoint"
            ])
        <*> intOption (mconcat
            [ short 'p'
            , long "port"
            , metavar "PORT"
            , value 14448
            , showDefault
            , help "Input port (output port will be PORT + 1)"
            ])
        <*> some (strArgument (metavar "PROG [ARG...]"))
      where
        intOption :: Mod OptionFields Int -> Parser Int
        intOption = option auto

main' :: Args -> IO ()
main' Args{..} = do
    -- Handles are not compatible with System.ZMQ4.Monadic.poll, so make raw
    -- file descriptors instead.
    (stdinReadFd,  stdinWriteFd)  <- createPipe
    (stdoutReadFd, stdoutWriteFd) <- createPipe
    (stderrReadFd, stderrWriteFd) <- createPipe

    runManaged $ do
        -- But, use handles everywhere else for convenience.
        stdinReadH   <- managed (withHandle stdinReadFd)
        stdinWriteH  <- managed (withHandle stdinWriteFd)
        stdoutReadH  <- managed (withHandle stdoutReadFd)
        stdoutWriteH <- managed (withHandle stdoutWriteFd)
        stderrReadH  <- managed (withHandle stderrReadFd)
        stderrWriteH <- managed (withHandle stderrWriteFd)

        liftIO $ do
            -- Don't buffer stdout/stderr so we can display things like prompts.
            hSetBuffering stdinReadH   LineBuffering
            hSetBuffering stdinWriteH  LineBuffering
            hSetBuffering stdoutReadH  NoBuffering
            hSetBuffering stdoutWriteH NoBuffering
            hSetBuffering stderrReadH  NoBuffering
            hSetBuffering stderrWriteH NoBuffering

            printf "Spawning '%s'\n" (unwords argCommand)

            let (x:xs) = argCommand
            void $ createProcess (proc x xs)
                       { std_in  = UseHandle stdinReadH
                       , std_out = UseHandle stdoutWriteH
                       , std_err = UseHandle stdoutWriteH
                       }

            runZMQ $ do
                subscriber <- mkSubscriber
                publisher  <- mkPublisher

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
  where
    mkSubscriber :: ZMQ z (Socket z Sub)
    mkSubscriber = do
        let subscriber_endpoint = printf "tcp://%s:%d" argEndpoint argPort
        liftIO $ printf "Binding input socket to '%s'\n" subscriber_endpoint

        subscriber <- socket Sub
        bind subscriber subscriber_endpoint
        subscribe subscriber ""
        return subscriber

    mkPublisher :: ZMQ z (Socket z Pub)
    mkPublisher = do
        let publisher_endpoint = printf "tcp://%s:%d" argEndpoint (argPort+1)
        liftIO $ printf "Binding output socket to '%s'\n" publisher_endpoint

        publisher <- socket Pub
        bind publisher publisher_endpoint
        return publisher

withHandle :: Fd -> (Handle -> IO a) -> IO a
withHandle fd = bracket (fdToHandle fd) hClose

-- | Get all available bytes on a socket.
hGetAvailable :: MonadIO m => Handle -> m ByteString
hGetAvailable h = do
    bytes <- liftIO $ BS.hGetNonBlocking h blockSize
    if BS.length bytes == blockSize
        then (bytes <>) <$> hGetAvailable h
        else return bytes
  where
    blockSize = 2048
