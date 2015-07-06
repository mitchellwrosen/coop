module Util where

import Control.Monad.Catch
import Data.ByteString.Char8 (ByteString)
import Data.List.NonEmpty
import Data.Typeable
import Options.Applicative

--------------------------------------------------------------------------------
-- Message parser

data ParseException = ParseException
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
parseMessageStdin _ = throwM ParseException

serializeMessage :: Message -> NonEmpty ByteString
serializeMessage (MsgStdin client_id msg) = ["STDIN", client_id, msg]
serializeMessage (MsgStdout msg) = ["STDOUT", msg]
serializeMessage (MsgStderr msg) = ["STDERR", msg]

--------------------------------------------------------------------------------
-- Shared options

endpointOption :: Parser String
endpointOption = strOption $ mconcat
    [ short 'e'
    , long "endpoint"
    , metavar "ENDPOINT"
    , value "127.0.0.1"
    , showDefault
    , help "Endpoint"
    ]

portOption :: Parser Int
portOption = intOption $ mconcat
    [ short 'p'
    , long "port"
    , metavar "PORT"
    , value 14448
    , showDefault
    , help "Port"
    ]
  where
    intOption :: Mod OptionFields Int -> Parser Int
    intOption = option auto
