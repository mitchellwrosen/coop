module Main where

import Client
import Server

import Options.Applicative

data Args = Args
    { argEndpoint :: String
    , argPort     :: Int
    , argCommand  :: [String]
    }

main :: IO ()
main = execParser opts >>= \Args{..} ->
    case argCommand of
        [] -> clientMain argEndpoint argPort
        _  -> serverMain argEndpoint argPort argCommand
  where
    opts :: ParserInfo Args
    opts = info (helper <*> parseArgs) $ mconcat
        [ fullDesc
        , progDesc "Start or connect to a server running <PROG ARG...> on <ENDPOINT:PORT>"
        , header "Coop v0.1.0.0"
        ]

    parseArgs :: Parser Args
    parseArgs = Args
        <$> endpointOption
        <*> portOption
        <*> many (strArgument (metavar "[PROG [ARG...]]"))

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

    intOption :: Mod OptionFields Int -> Parser Int
    intOption = option auto
