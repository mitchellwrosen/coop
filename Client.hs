module Main where

import Util

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Char8    (ByteString)
import qualified Data.ByteString.Char8    as BS
import           Data.Function            (fix)
import           Data.Monoid
import           System.IO
import           System.Posix.IO          (stdInput)
import           System.Random
import           System.ZMQ4.Monadic

main :: IO ()
main = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering

    runZMQ $ do
        publisher <- socket Pub
        connect publisher "tcp://127.0.0.1:9000"
        client_id <- mkRandomId
        setIdentity (restrict client_id) publisher

        subscriber <- socket Sub
        connect subscriber "tcp://127.0.0.1:9001"
        subscribe subscriber ""

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

-- | Make random 4 character ID.
mkRandomId :: MonadIO m => m ByteString
mkRandomId = BS.pack . take 4 . randomRs ('a','z') <$> liftIO getStdGen
