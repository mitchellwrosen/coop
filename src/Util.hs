module Util where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List.NonEmpty    (NonEmpty)
import           Data.Typeable
import           Options.Applicative
import           System.IO
import           System.Random

--------------------------------------------------------------------------------
-- Message parser

data ParseException = ParseException [ByteString]
    deriving (Show, Typeable)

instance Exception ParseException

type ClientId = ByteString

data Message
    = MsgStdin ClientId ByteString
    | MsgStdout ByteString
    | MsgStderr ByteString

parseMessage :: MonadThrow m => [ByteString] -> m Message
parseMessage ["STDOUT", msg] = return (MsgStdout msg)
parseMessage ["STDERR", msg] = return (MsgStderr msg)
parseMessage msg = parseMessageStdin msg

parseMessageStdin :: MonadThrow m => [ByteString] -> m Message
parseMessageStdin ["STDIN", client_id, msg] = return (MsgStdin client_id msg)
parseMessageStdin msgs = throwM (ParseException msgs)

serializeMessage :: Message -> NonEmpty ByteString
serializeMessage (MsgStdin client_id msg) = ["STDIN", client_id, msg]
serializeMessage (MsgStdout msg) = ["STDOUT", msg]
serializeMessage (MsgStderr msg) = ["STDERR", msg]

displayMessage :: MonadIO m => Message -> m ()
displayMessage (MsgStdin client_id msg) = liftIO . BS.putStrLn $ "[" <> client_id <> "]: " <> msg
displayMessage (MsgStdout msg) = liftIO $ BS.putStr msg
displayMessage (MsgStderr msg) = liftIO $ BS.hPutStr stderr msg

--------------------------------------------------------------------------------
-- Misc

-- | Make random 4 character ID.
mkRandomId :: MonadIO m => m ByteString
mkRandomId = BS.pack . take 4 . randomRs ('a','z') <$> liftIO getStdGen
