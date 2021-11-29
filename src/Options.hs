module Options (Options (..), parseOptions) where

import Options.Applicative (Parser, auto, execParser, fullDesc, help, helper, info, long, metavar, option, short, showDefault, strOption, value, (<**>))

data Options = Options
    { serverPort :: Int
    , imageFolder :: String
    , dbConnectionString :: String
    }

options :: Parser Options
options =
    Options
        <$> option
            auto
            ( long "port"
                <> short 'p'
                <> help "Server port"
                <> showDefault
                <> value 9999
                <> metavar "INT"
            )
        <*> strOption
            ( long "image-folder"
                <> help "Folder where images are kept"
                <> showDefault
                <> value "marketplace-images"
                <> metavar "STR"
            )
        <*> strOption
            ( long "db-connection"
                <> help "LibPQ connection string"
                <> metavar "STR"
            )

parseOptions :: IO Options
parseOptions = execParser (info (options <**> helper) fullDesc)
