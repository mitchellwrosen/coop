module Util where

import Control.Monad.Catch
import Data.ByteString.Char8 (ByteString)
import Data.List.NonEmpty
import Data.Typeable

data ParseException = ParseException
    deriving (Show, Typeable)

instance Exception ParseException

type ClientId = ByteString

data Outfd = Stdout | Stderr

data Message
    = Request ClientId ByteString
    | Response Outfd ByteString

parseMessage :: MonadThrow m => [ByteString] -> m Message
parseMessage ["RESPONSE", "STDOUT", msg] = return (Response Stdout msg)
parseMessage ["RESPONSE", "STDERR", msg] = return (Response Stderr msg)
parseMessage msg = parseRequest msg

parseRequest :: MonadThrow m => [ByteString] -> m Message
parseRequest ["REQUEST", client_id, msg] = return (Request client_id msg)
parseRequest _ = throwM ParseException

serializeMessage :: Message -> NonEmpty ByteString
serializeMessage (Request client_id msg) = ["REQUEST", client_id, msg]
serializeMessage (Response Stdout msg)   = ["RESPONSE", "STDOUT", msg]
serializeMessage (Response Stderr msg)   = ["RESPONSE", "STDERR", msg]

