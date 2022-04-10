{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-| Request functions & response types for the Gemini Exchange API.
-}
module Web.Gemini
    ( GeminiApiM
    , runApi
    , GeminiConfig(..)
    -- * Requests
    -- ** Symbol Details
    , getSymbolDetails
    , SymbolDetails(..)
    -- ** Trade History
    , getMyTrades
    , Trade(..)
    -- ** Transfer History
    , getMyTransfers
    , Transfer(..)
    -- * Helpers
    , protectedGeminiRequest
    , createSignature
    , makeNonce
    ) where

import           Control.Exception.Safe         ( MonadCatch
                                                , MonadThrow
                                                )
import           Control.Monad.Reader           ( MonadIO(liftIO)
                                                , MonadReader(ask)
                                                , ReaderT(..)
                                                , lift
                                                )
import           Crypto.Hash                    ( SHA384 )
import           Crypto.MAC.HMAC                ( hmac
                                                , hmacGetDigest
                                                )
import           Data.Aeson                     ( (.:)
                                                , (.:?)
                                                , FromJSON(..)
                                                , ToJSON
                                                , Value(..)
                                                , encode
                                                , withObject
                                                )
import           Data.ByteString.Base64         ( encodeBase64 )
import           Data.Scientific                ( Scientific )
import           Data.Text                      ( Text )
import           Data.Text.Encoding             ( encodeUtf8 )
import           Data.Time                      ( secondsToNominalDiffTime )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                )
import           Data.Version                   ( showVersion )
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Req               ( (/:)
                                                , GET(..)
                                                , HttpBodyAllowed
                                                , HttpMethod(..)
                                                , JsonResponse
                                                , MonadHttp(..)
                                                , NoReqBody(..)
                                                , Option
                                                , POST(..)
                                                , ProvidesBody
                                                , Req
                                                , Url
                                                , defaultHttpConfig
                                                , header
                                                , https
                                                , jsonResponse
                                                , req
                                                , responseBody
                                                , runReq
                                                )

import           Paths_gemini_exports           ( version )

import qualified Data.Aeson.KeyMap             as KM
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BC
import qualified Data.ByteString.Lazy          as LBS

-- | Required configuration data for making requests to the Gemini API.
data GeminiConfig = GeminiConfig
    { gcApiKey    :: Text
    , gcApiSecret :: Text
    }
    deriving (Show, Read, Eq, Ord)

-- | Monad in which Gemini API requests are run.
newtype GeminiApiM a = GeminiApiM
    { runGeminiApiM :: ReaderT GeminiConfig Req a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader GeminiConfig, MonadThrow, MonadCatch)

-- | Run a series of API requests with the given Config.
runApi :: GeminiConfig -> GeminiApiM a -> IO a
runApi cfg = runReq defaultHttpConfig . flip runReaderT cfg . runGeminiApiM

-- | Use 'MonadHttp' from the 'Req' monad.
instance MonadHttp GeminiApiM where
    handleHttpException = GeminiApiM . lift . handleHttpException


-- SYMBOL DETAILS

-- | Fetch the details on a supported symbol.
getSymbolDetails :: MonadHttp m => Text -> m SymbolDetails
getSymbolDetails symbol =
    responseBody
        <$> req
                GET
                (  https "api.gemini.com"
                /: "v1"
                /: "symbols"
                /: "details"
                /: symbol
                )
                NoReqBody
                jsonResponse
                userAgentHeader

-- | Currency & Precision details for a 'Trade' Symbol.
data SymbolDetails = SymbolDetails
    { sdSymbol         :: Text
    , sdBaseCurrency   :: Text
    , sdBasePrecision  :: Scientific
    , sdQuoteCurrency  :: Text
    , sdQuotePrecision :: Scientific
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON SymbolDetails where
    parseJSON = withObject "SymbolDetails" $ \o -> do
        sdSymbol         <- o .: "symbol"
        sdBaseCurrency   <- o .: "base_currency"
        sdBasePrecision  <- o .: "tick_size"
        sdQuoteCurrency  <- o .: "quote_currency"
        sdQuotePrecision <- o .: "quote_increment"
        return SymbolDetails { .. }


-- TRADE HISTORY

-- | Fetch all my Gemini Trades
getMyTrades :: GeminiApiM [Trade]
getMyTrades = do
    nonce <- makeNonce
    -- TODO: take optional start & optional end, use start to set
    -- `timestamp`, use end to determine when to stop fetching.
    -- TODO: if response gives 500 results, shift start time & continue
    -- fetching until received empty list.
    let parameters = KM.fromList
            [ ("request"     , String "/v1/mytrades")
            , ("nonce"       , Number $ fromInteger nonce)
            , ("limit_trades", Number 500)
            ]
    responseBody
        <$> protectedGeminiRequest
                POST
                (https "api.gemini.com" /: "v1" /: "mytrades")
                parameters

-- | A single, completed Trade.
data Trade = Trade
    { tId          :: Integer
    , tSymbol      :: Text
    , tPrice       :: Scientific
    , tAmount      :: Scientific
    , tFeeCurrency :: Text
    , tFeeAmount   :: Scientific
    , tIsBuy       :: Bool
    , tIsAggressor :: Bool
    , tTimestamp   :: POSIXTime
    , tOrderId     :: Text
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Trade where
    parseJSON = withObject "Trade" $ \o -> do
        tId          <- o .: "tid"
        tSymbol      <- o .: "symbol"
        tPrice       <- read <$> o .: "price"
        tAmount      <- read <$> o .: "amount"
        tFeeCurrency <- o .: "fee_currency"
        tFeeAmount   <- read <$> o .: "fee_amount"
        tIsBuy       <- (== ("Buy" :: String)) <$> o .: "type"
        tIsAggressor <- o .: "aggressor"
        tTimestamp   <- (/ 1000.0) <$> o .: "timestampms"
        tOrderId     <- o .: "order_id"
        return Trade { .. }


-- TRANSFER HISTORY

-- | Fetch all my Gemini Transfers
getMyTransfers :: GeminiApiM [Transfer]
getMyTransfers = do
    nonce <- makeNonce
    -- TODO: take optional start & optional end, use start to set
    -- `timestamp`, use end to determine when to stop fetching.
    -- TODO: if response gives 500 results, shift start time & continue
    -- fetching until received empty list.
    let parameters = KM.fromList
            [ ("request"        , String "/v1/transfers")
            , ("nonce"          , Number $ fromInteger nonce)
            , ("limit_transfers", Number 50)
            ]
    responseBody
        <$> protectedGeminiRequest
                POST
                (https "api.gemini.com" /: "v1" /: "transfers")
                parameters

-- | A single fiat or cryptocurrency transfer, credit, deposit, or withdrawal.
data Transfer = Transfer
    { trId        :: Integer
    , trType      :: Text
    , trStatus    :: Text
    , trCurrency  :: Text
    , trAmount    :: Scientific
    , trMethod    :: Maybe Text
    , trTimestamp :: POSIXTime
    }
    deriving (Show, Read, Eq, Ord, Generic)

instance FromJSON Transfer where
    parseJSON = withObject "Transfer" $ \o -> do
        trId        <- o .: "eid"
        trType      <- o .: "type"
        trStatus    <- o .: "status"
        trCurrency  <- o .: "currency"
        trAmount    <- read <$> o .: "amount"
        trMethod    <- o .:? "method"
        trTimestamp <-
            secondsToNominalDiffTime . (/ 1000) <$> o .: "timestampms"
        return Transfer { .. }


-- UTILS

-- | Run a request that requires authorization against the Gemini API.
protectedGeminiRequest
    :: ( MonadHttp m
       , HttpMethod method
       , HttpBodyAllowed (AllowsBody method) (ProvidesBody NoReqBody)
       , ToJSON body
       , FromJSON response
       , MonadReader GeminiConfig m
       )
    => method
    -> Url scheme
    -> body
    -> m (JsonResponse response)
protectedGeminiRequest method url body = do
    cfg <- ask
    let payload   = encodeUtf8 . encodeBase64 . LBS.toStrict $ encode body
        signature = createSignature cfg payload
    let authorizedOptions = mconcat
            [ header "Content-Type"       "text/plain"
            , header "X-GEMINI-APIKEY"    (encodeUtf8 $ gcApiKey cfg)
            , header "X-GEMINI-PAYLOAD"   payload
            , header "X-GEMINI-SIGNATURE" signature
            , header "Cache-Control"      "no-cache"
            , userAgentHeader
            ]
    req method url NoReqBody jsonResponse authorizedOptions


-- | Generate a 'Crypto.MAC.HMAC.HMAC' 'SHA384' signature for an authorized
-- API request.
createSignature
    :: GeminiConfig
    -- ^ API Credentials
    -> BS.ByteString
    -- ^ Base64-encoded request body.
    -> BS.ByteString
createSignature cfg body =
    let digest =
            hmacGetDigest @SHA384 $ hmac (encodeUtf8 $ gcApiSecret cfg) body
    in  BC.pack $ show digest


-- | Generate a nonce for authorized requests from the current timestamp in
-- milliseconds.
makeNonce :: MonadIO m => m Integer
makeNonce = truncate . (1000 *) <$> liftIO getPOSIXTime


-- | Generate a @User-Agent@ header with the library's current version.
userAgentHeader :: Option scheme
userAgentHeader =
    header "User-Agent" . BC.pack $ "gemini-exports/v" <> showVersion version
