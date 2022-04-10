{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{- | CLI application harness.

-}
module Console.Gemini.Exports.Main
    ( run
    , getArgs
    , Args(..)
    ) where

import           Control.Monad                  ( forM )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Csv                       ( (.=)
                                                , DefaultOrdered(..)
                                                , ToNamedRecord(..)
                                                , defaultEncodeOptions
                                                , encUseCrLf
                                                , encodeDefaultOrderedByNameWith
                                                , header
                                                , namedRecord
                                                )
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Data.Scientific                ( FPFormat(Fixed)
                                                , formatScientific
                                                )
import           Data.Text                      ( Text )
import           Data.Time                      ( TimeZone
                                                , defaultTimeLocale
                                                , formatTime
                                                , getTimeZone
                                                , utcToZonedTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Data.Version                   ( showVersion )
import           System.Console.CmdArgs         ( (&=)
                                                , Data
                                                , Typeable
                                                , cmdArgs
                                                , def
                                                , explicit
                                                , help
                                                , helpArg
                                                , name
                                                , program
                                                , summary
                                                , typ
                                                )
import           System.Exit                    ( exitFailure )
import           System.IO                      ( hPutStrLn
                                                , stderr
                                                )

import           Paths_gemini_exports           ( version )
import           Web.Gemini

import qualified Data.ByteString.Lazy.Char8    as LBS
import qualified Data.List                     as L
import qualified Data.Map.Strict               as M


-- | Run the executable.
run :: Args -> IO ()
run cfgArgs = do
    AppConfig {..} <- makeConfig cfgArgs
    exportData     <- runApi geminiCfg $ do
        trades <- getMyTrades
        let symbols = L.nub $ map tSymbol trades
        symbolDetails <- fmap M.fromList . forM symbols $ \symbol -> do
            (symbol, ) <$> getSymbolDetails symbol
        fmap catMaybes . forM trades $ \t -> do
            tz <- liftIO . getTimeZone . posixSecondsToUTCTime $ tTimestamp t
            return $ ExportData tz t <$> M.lookup (tSymbol t) symbolDetails
    let csvData = encodeDefaultOrderedByNameWith
            (defaultEncodeOptions { encUseCrLf = False })
            exportData
    if outputFile == "-"
        then LBS.putStrLn csvData
        else LBS.writeFile outputFile csvData


-- | The data required for rendering a single CSV row.
data ExportData = ExportData TimeZone Trade SymbolDetails
    deriving (Show, Read, Eq, Ord)

instance DefaultOrdered ExportData where
    headerOrder _ = header
        [ "time"
        , "base-asset"
        , "quote-asset"
        , "type"
        , "price"
        , "quantity"
        , "total"
        , "fee"
        , "fee-currency"
        , "trade-id"
        ]

instance ToNamedRecord ExportData where
    toNamedRecord (ExportData tz Trade {..} SymbolDetails {..}) = namedRecord
        [ "time" .= formatTime
            defaultTimeLocale
            "%F %T%Q"
            (utcToZonedTime tz $ posixSecondsToUTCTime tTimestamp)
        , "base-asset" .= sdBaseCurrency
        , "quote-asset" .= sdQuoteCurrency
        , "type" .= if tIsBuy then "Buy" else ("Sell" :: Text)
        , "price" .= formatScientific Fixed Nothing tPrice
        , "quantity" .= formatScientific Fixed Nothing tAmount
        , "total" .= formatScientific Fixed Nothing (tPrice * tAmount)
        , "fee" .= formatScientific Fixed Nothing tFeeAmount
        , "fee-currency" .= tFeeCurrency
        , "trade-id" .= tId
        ]


data AppConfig = AppConfig
    { geminiCfg  :: GeminiConfig
    , outputFile :: FilePath
    }
    deriving (Show, Read, Eq, Ord)

makeConfig :: Args -> IO AppConfig
makeConfig Args {..} = do
    gcApiKey <- case argApiKey of
        Nothing ->
            hPutStrLn stderr "[ERROR] Pass a Gemini API Key with `-k`."
                >> exitFailure
        Just k -> return k
    gcApiSecret <- case argApiSecret of
        Nothing ->
            hPutStrLn stderr "[ERROR] Pass a Gemini API Secret with `-s`."
                >> exitFailure
        Just s -> return s
    let geminiCfg = GeminiConfig { .. }
    return $ AppConfig { outputFile = fromMaybe "-" argOutputFile, .. }


-- | CLI arguments supported by the executable.
data Args = Args
    { argApiKey     :: Maybe Text
    , argApiSecret  :: Maybe Text
    , argOutputFile :: Maybe FilePath
    }
    deriving (Show, Read, Eq, Data, Typeable)


-- | Parse the CLI arguments with 'System.Console.CmdArgs'.
getArgs :: IO Args
getArgs = cmdArgs argSpec


-- | Defines & documents the CLI arguments.
argSpec :: Args
argSpec =
    Args
            { argApiKey     = def
                              &= name "api-key"
                              &= name "k"
                              &= explicit
                              &= help "Gemini API Key"
                              &= typ "KEY"
            , argApiSecret  = def
                              &= name "api-secret"
                              &= name "s"
                              &= explicit
                              &= help "Gemini API Secret"
                              &= typ "SECRET"
            , argOutputFile = Nothing
                              &= help "File to write export to. Default: stdout"
                              &= name "o"
                              &= name "output-file"
                              &= explicit
                              &= typ "FILE"
            }
        &= summary
               (  "gemini-exports v"
               <> showVersion version
               <> ", Pavan Rikhi 2022"
               )
        &= program "gemini-exports"
        &= helpArg [name "h"]
        &= help "Generate CSV Exports of your Gemini Trades."
