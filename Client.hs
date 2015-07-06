module Main where

import Util

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Function            (fix)
import           Data.Monoid
import           Options.Applicative
import           System.IO
import           System.Posix.IO          (stdInput)
import           System.Random
import           System.ZMQ4.Monadic
import           Text.Printf

data Args = Args
    { argEndpoint :: String
    , argPort     :: Int
    }

main :: IO ()
main = execParser opts >>= main'
  where
    opts :: ParserInfo Args
    opts = info (helper <*> parseArgs) $ mconcat
        [ fullDesc
        , progDesc "Connect to a server on <ENDPOINT:PORT>"
        , header "Coop client v0.1.0.0"
        ]

    parseArgs :: Parser Args
    parseArgs = Args <$> endpointOption <*> portOption

main' :: Args -> IO ()
main' Args{..} = runZMQ $ do
    liftIO $ hSetBuffering stdin  LineBuffering
    liftIO $ hSetBuffering stdout NoBuffering

    client_id <- mkRandomId

    publisher  <- mkPublisher client_id
    subscriber <- mkSubscriber

    let in_callback _ =
            liftIO BS.getLine >>=
              sendMulti publisher . serializeMessage . MsgStdin client_id

        out_callback _ =
            receiveMulti subscriber >>= parseMessage >>= \case
                MsgStdin client_id' msg ->
                    unless (client_id == client_id') $
                        liftIO . BS.putStrLn $ "[" <> client_id' <> "]: " <> msg

                MsgStdout msg ->
                    liftIO (BS.putStr msg)

                MsgStderr msg ->
                    liftIO (BS.putStr msg)

    fix $ \loop -> do
        evts <- concat <$>
            poll (-1) [ File stdInput   [In] (Just in_callback)
                      , Sock subscriber [In] (Just out_callback)
                      ]
        unless (Err `elem` evts)
            loop
  where
    mkPublisher :: ByteString -> ZMQ z (Socket z Pub)
    mkPublisher client_id = do
        let publisher_endpoint = printf "tcp://%s:%d" argEndpoint argPort
        liftIO $ printf "Publishing input on '%s'\n" publisher_endpoint

        publisher <- socket Pub
        connect publisher publisher_endpoint
        setIdentity (restrict client_id) publisher
        return publisher

    mkSubscriber :: ZMQ z (Socket z Sub)
    mkSubscriber = do
        let subscriber_endpoint = printf "tcp://%s:%d" argEndpoint (argPort+1)
        liftIO $ printf "Subscribing to output on '%s'\n" subscriber_endpoint

        subscriber <- socket Sub
        connect subscriber subscriber_endpoint
        subscribe subscriber ""
        return subscriber

-- | Make random 4 character ID.
mkRandomId :: MonadIO m => m ByteString
mkRandomId = BS.pack . take 4 . randomRs ('a','z') <$> liftIO getStdGen
