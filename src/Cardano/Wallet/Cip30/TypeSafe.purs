module Cardano.Wallet.Cip30.TypeSafe
  ( APIErrorTag
      ( APIErrorInvalidRequest
      , APIErrorInternalError
      , APIErrorRefused
      , APIErrorAccountChange
      )
  , APIError
  , DataSignErrorTag
      ( DataSignErrorProofGeneration
      , DataSignErrorAddressNotPK
      , DataSignErrorUserDeclined
      )
  , DataSignError
  , TxSendErrorTag(TxSendErrorRefused, TxSendErrorFailure)
  , TxSendError
  , TxSignErrorTag(TxSignErrorProofGeneration, TxSignErrorUserDeclined)
  , TxSignError
  , enable
  , isEnabled
  , getExtensions
  , getNetworkId
  , getUtxos
  , getCollateral
  , getBalance
  , getUsedAddresses
  , getUnusedAddresses
  , getChangeAddress
  , getRewardAddresses
  , signTx
  , signData
  , submitTx
  , _apiError
  , _txSignError
  , _dataSignError
  , _txSendError
  , _paginateError
  , _success
  ) where

import Prelude

import Cardano.Wallet.Cip30
  ( Api
  , Bytes
  , Cbor
  , DataSignature
  , Extension
  , NetworkId
  , Paginate
  )
import Cardano.Wallet.Cip30
  ( Extension
  , WalletName
  , enable
  , getBalance
  , getChangeAddress
  , getCollateral
  , getExtensions
  , getNetworkId
  , getRewardAddresses
  , getUnusedAddresses
  , getUsedAddresses
  , getUtxos
  , isEnabled
  , signData
  , signTx
  , submitTx
  ) as Cip30
import Control.Alt ((<|>))
import Control.Monad.Error.Class (liftMaybe)
import Control.Monad.Except (throwError)
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, expand, inj)
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Effect.Exception (Error, error, message)
import Prim.Row (class Union)
import Type.Proxy (Proxy(Proxy))

-- CIP-30 errors as purescript types

data APIErrorTag
  = APIErrorInvalidRequest
  | APIErrorInternalError
  | APIErrorRefused
  | APIErrorAccountChange

derive instance Eq APIErrorTag
derive instance Generic APIErrorTag _

instance Show APIErrorTag where
  show = genericShow

type APIError =
  { code :: APIErrorTag
  , info :: String
  }

data DataSignErrorTag
  = DataSignErrorProofGeneration
  | DataSignErrorAddressNotPK
  | DataSignErrorUserDeclined

derive instance Eq DataSignErrorTag
derive instance Generic DataSignErrorTag _

instance Show DataSignErrorTag where
  show = genericShow

type DataSignError =
  { code :: DataSignErrorTag
  , info :: String
  }

data TxSendErrorTag = TxSendErrorRefused | TxSendErrorFailure

derive instance Eq TxSendErrorTag
derive instance Generic TxSendErrorTag _

instance Show TxSendErrorTag where
  show = genericShow

type TxSendError =
  { code :: TxSendErrorTag
  , info :: String
  }

data TxSignErrorTag = TxSignErrorProofGeneration | TxSignErrorUserDeclined

derive instance Eq TxSignErrorTag
derive instance Generic TxSignErrorTag _

instance Show TxSignErrorTag where
  show = genericShow

type TxSignError =
  { code :: TxSignErrorTag
  , info :: String
  }

type PaginateError = { maxSize :: Int }

-- Implementations of CIP-30 functions

enable
  :: Cip30.WalletName
  -> Array Cip30.Extension
  -> Aff (Variant (success :: Api, apiError :: APIError))
enable wallet extensions = catchCode "enable" (Cip30.enable wallet extensions)
  (inj _success)
  apiErrorMatcher

isEnabled
  :: Cip30.WalletName
  -> Aff (Variant (success :: Boolean, apiError :: APIError))
isEnabled wallet = catchCode "isEnabled" (Cip30.isEnabled wallet) (inj _success)
  apiErrorMatcher

getExtensions
  :: Api
  -> Aff (Variant (apiError :: APIError, success :: Array Extension))
getExtensions api = catchCode "getExtensions" (Cip30.getExtensions api)
  (inj _success)
  apiErrorMatcher

getNetworkId
  :: Api
  -> Aff (Variant (apiError :: APIError, success :: NetworkId))
getNetworkId api = catchCode "getNetworkId" (Cip30.getNetworkId api)
  (inj _success)
  apiErrorMatcher

getUtxos
  :: Api
  -> Maybe Cbor
  -> Maybe Paginate
  -> Aff
       ( Variant
           ( apiError :: APIError
           , paginateError :: PaginateError
           , success :: Maybe (Array Cbor)
           )
       )
getUtxos api amount paginate = catchCode "getUtxos"
  (Cip30.getUtxos api amount paginate)
  (inj _success)
  (apiErrorMatcher `combineErrorMatchers` paginateErrorMatcher)

getCollateral
  :: Api
  -> Cbor
  -> Aff (Variant (success :: Maybe (Array Cbor), apiError :: APIError))
getCollateral api cbor = catchCode "getCollateral"
  (Cip30.getCollateral api cbor)
  (inj _success)
  apiErrorMatcher

getBalance
  :: Api -> Aff (Variant (success :: Cbor, apiError :: APIError))
getBalance api = catchCode "getBalance" (Cip30.getBalance api) (inj _success)
  apiErrorMatcher

getUsedAddresses
  :: Api
  -> Maybe Paginate
  -> Aff
       ( Variant
           ( success :: Array Cbor
           , paginateError :: PaginateError
           , apiError :: APIError
           )
       )
getUsedAddresses api paginate = catchCode "getUsedAddresses"
  (Cip30.getUsedAddresses api paginate)
  (inj _success)
  (apiErrorMatcher `combineErrorMatchers` paginateErrorMatcher)

getUnusedAddresses
  :: Api
  -> Aff (Variant (success :: Array Cbor, apiError :: APIError))
getUnusedAddresses api = catchCode "getUnusedAddresses"
  (Cip30.getUnusedAddresses api)
  (inj _success)
  apiErrorMatcher

getChangeAddress
  :: Api -> Aff (Variant (success :: Cbor, apiError :: APIError))
getChangeAddress api = catchCode "getChangeAddress" (Cip30.getChangeAddress api)
  (inj _success)
  apiErrorMatcher

getRewardAddresses
  :: Api
  -> Aff (Variant (success :: Array Cbor, apiError :: APIError))
getRewardAddresses api = catchCode "getRewardAddresses"
  (Cip30.getRewardAddresses api)
  (inj _success)
  apiErrorMatcher

signTx
  :: Api
  -> Cbor
  -> Boolean
  -> Aff
       ( Variant
           (apiError :: APIError, txSignError :: TxSignError, success :: Cbor)
       )
signTx api tx isPartialSign =
  catchCode "signTx" (Cip30.signTx api tx isPartialSign) (inj _success)
    (apiErrorMatcher `combineErrorMatchers` txSignErrorMatcher)

signData
  :: Api
  -> Cbor
  -> Bytes
  -> Aff
       ( Variant
           ( success :: DataSignature
           , dataSignError :: DataSignError
           , apiError :: APIError
           )
       )
signData api addr payload = catchCode "signData"
  (Cip30.signData api addr payload)
  (inj _success)
  (apiErrorMatcher `combineErrorMatchers` dataSignErrorMatcher)

submitTx
  :: Api
  -> Cbor
  -> Aff
       ( Variant
           (success :: String, apiError :: APIError, txSendError :: TxSendError)
       )
submitTx api tx = catchCode "submitTx" (Cip30.submitTx api tx) (inj _success)
  (apiErrorMatcher `combineErrorMatchers` txSendErrorMatcher)

-- Error matching machinery

-- | A known error is either a pagination error or an error with code and
-- | message.
-- | This type represents error info extracted at runtime, that is yet
-- | uninterpreted.
-- | `ErrorMatcher` can be used to dispatch on these values, taking the
-- | current CIP-30 endpoint into account.
type ErrorData = Either { maxSize :: Int } { code :: Int, info :: String }

-- | An `ErrorMatcher` is a function that tries to match a known error with
-- | an error `Variant` based on `ErrorData`.
newtype ErrorMatcher (row :: Row Type) = ErrorMatcher
  (ErrorData -> Maybe (Variant row))

-- | `ErrorMatcher`s can be joined: e.g `APIError` and `TxSignError` have
-- | non-intersecting error codes, so we can build an `ErrorMatcher` dispatcher
-- | that tries first and then the second.
combineErrorMatchers
  :: forall row1 row2 row3
   . Union row1 row2 row3
  => Union row2 row1 row3
  => ErrorMatcher row1
  -> ErrorMatcher row2
  -> ErrorMatcher row3
combineErrorMatchers (ErrorMatcher f1) (ErrorMatcher f2) =
  ErrorMatcher \errorData ->
    expand <$> f1 errorData <|> expand <$> f2 errorData

-- | Uses `ErrorMatcher` to transform a `purescript-cip30` function into a
-- | function 'enriched' with error type variants. Arguments:
-- |
-- | - CIP-30 method name
-- | - `Aff` action
-- | - A function that injects successful result into the row
-- | - `ErrorMatcher` that captures known errors
-- |
-- | It works like this:
-- | - call the `Aff` action
-- |   - If no exception, inject it into `success` variant.
-- |   - if there is an exception, run the error matcher
-- |     - If it is successful, return the "enriched" `(Variant row)`
-- |     - If it fails, re-throw the error
catchCode
  :: forall a errorRow row
   . Union errorRow (success :: a) row
  => String
  -> Aff a
  -> (a -> Variant row)
  -> (ErrorMatcher errorRow)
  -> Aff (Variant row)
catchCode functionName action handleSuccess (ErrorMatcher handleException) = do
  -- run the action
  (action >>= handleSuccess >>> pure)
    `catchError` \errorValue -> do
      -- extract all needed information from the thrown value
      mbErrorTagInt <- liftEffect $ _getErrorTagInt Nothing Just errorValue
      mbErrorInfoString <- liftEffect $ _getErrorInfoString Nothing Just
        errorValue
      mbPaginateErrorMaxSize <- liftEffect $ _getPaginateError Nothing Just
        errorValue
      let
        mbErrorData =
          -- Figure out which type of ErrorData is this, if any
          case mbErrorTagInt, mbErrorInfoString, mbPaginateErrorMaxSize of
            -- error with error code and info message
            Just errorTagInt, Just errorInfoString, _ ->
              Just (Right { code: errorTagInt, info: errorInfoString })
            -- pagination error
            _, _, Just maxSize ->
              Just (Left { maxSize })
            -- unknown error we can't dispatch on
            _, _, _ -> Nothing
        -- will be thrown if we can't provide a recoverable error
        myBad = error $ "CIP-30 " <> functionName
          <> ": unable to match error with specification: "
          <> message errorValue
      exception <- liftMaybe myBad mbErrorData
      -- run the matcher and see if it is able to recover the error
      case expand <$> handleException exception of
        Nothing -> liftEffect $ throwError myBad
        Just res -> pure res

-- | Tries to get `error.code` number
foreign import _getErrorTagInt
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Error
  -> Effect (Maybe Int)

-- | Tries to get `error.code` number
foreign import _getErrorInfoString
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Error
  -> Effect (Maybe String)

-- | Tries to get `error.maxSize` number
foreign import _getPaginateError
  :: (forall a. Maybe a)
  -> (forall a. a -> Maybe a)
  -> Error
  -> Effect (Maybe Int)

-- Error matchers. They correspond to error types in CIP-30

apiErrorMatcher :: ErrorMatcher (apiError :: APIError)
apiErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: (-1) } -> match APIErrorInvalidRequest info
    Right { info, code: (-2) } -> match APIErrorInternalError info
    Right { info, code: (-3) } -> match APIErrorRefused info
    Right { info, code: (-4) } -> match APIErrorAccountChange info
    _ -> skip
  where
  match err info = Just $ inj _apiError { info, code: err }
  skip = Nothing

txSignErrorMatcher :: ErrorMatcher (txSignError :: TxSignError)
txSignErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: 1 } -> match TxSignErrorProofGeneration info
    Right { info, code: 2 } -> match TxSignErrorUserDeclined info
    _ -> skip
  where
  match err info = Just $ inj _txSignError { info, code: err }
  skip = Nothing

dataSignErrorMatcher :: ErrorMatcher (dataSignError :: DataSignError)
dataSignErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: 1 } -> match DataSignErrorProofGeneration info
    Right { info, code: 2 } -> match DataSignErrorAddressNotPK info
    Right { info, code: 3 } -> match DataSignErrorUserDeclined info
    _ -> skip
  where
  match err info = Just $ inj _dataSignError
    { info, code: err }
  skip = Nothing

txSendErrorMatcher :: ErrorMatcher (txSendError :: TxSendError)
txSendErrorMatcher = ErrorMatcher
  case _ of
    Left _ -> skip
    Right { info, code: 1 } -> match TxSendErrorRefused info
    Right { info, code: 2 } -> match TxSendErrorFailure info
    _ -> skip
  where
  match err info = Just $ inj _txSendError { info, code: err }
  skip = Nothing

paginateErrorMatcher :: ErrorMatcher (paginateError :: PaginateError)
paginateErrorMatcher = ErrorMatcher
  case _ of
    Left maxSize -> Just $ inj _paginateError maxSize
    _ -> Nothing

-- Proxy values for convenient pattern matching via Variant

_success :: Proxy "success"
_success = Proxy

_apiError :: Proxy "apiError"
_apiError = Proxy

_txSignError :: Proxy "txSignError"
_txSignError = Proxy

_dataSignError :: Proxy "dataSignError"
_dataSignError = Proxy

_txSendError :: Proxy "txSendError"
_txSendError = Proxy

_paginateError :: Proxy "paginateError"
_paginateError = Proxy
