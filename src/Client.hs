module Client where

import Util

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8  as BS
import           Data.Function          (fix)
import           System.IO
import           System.Posix.IO        (stdInput)
import           System.ZMQ4.Monadic
import           Text.Printf

clientMain :: String -> Int -> IO ()
clientMain endpoint port = runZMQ $ do
    liftIO $ hSetBuffering stdin  LineBuffering
    liftIO $ hSetBuffering stdout NoBuffering

    client_id  <- mkRandomId
    publisher  <- mkPublisher
    subscriber <- mkSubscriber

    let stdin_callback _ =
            liftIO BS.getLine >>=
              sendMulti publisher . serializeMessage . MsgStdin client_id

        server_out_callback _ =
            receiveMulti subscriber >>= parseMessage >>= \case
                msg@(MsgStdin client_id' _) ->
                    unless (client_id == client_id') $
                        displayMessage msg
                msg -> displayMessage msg

    fix $ \loop -> do
        evts <- concat <$>
            poll (-1) [ File stdInput   [In] (Just stdin_callback)
                      , Sock subscriber [In] (Just server_out_callback)
                      ]
        unless (Err `elem` evts)
            loop
  where
    mkPublisher :: ZMQ z (Socket z Pub)
    mkPublisher = do
        let publisher_endpoint = printf "tcp://%s:%d" endpoint port
        liftIO $ printf "Publishing input on '%s'\n" publisher_endpoint

        publisher <- socket Pub
        connect publisher publisher_endpoint
        return publisher

    mkSubscriber :: ZMQ z (Socket z Sub)
    mkSubscriber = do
        let subscriber_endpoint = printf "tcp://%s:%d" endpoint (port+1)
        liftIO $ printf "Subscribing to output on '%s'\n" subscriber_endpoint

        subscriber <- socket Sub
        connect subscriber subscriber_endpoint
        subscribe subscriber ""
        return subscriber
