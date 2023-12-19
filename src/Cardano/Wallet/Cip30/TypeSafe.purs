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
  ) where

import Prelude

import Cardano.Wallet.Cip30
  ( Bytes
  , Cbor
  , Cip30Connection
  , Cip30DataSignature
  , Cip30Extension
  , NetworkId
  , Paginate
  )
import Cardano.Wallet.Cip30
  ( Cip30Extension
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
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Variant (Variant, expand, inj)
import Effect (Effect)
import Effect.Aff (Aff, catchError)
import Effect.Class (liftEffect)
import Effect.Exception (Error, throw)
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

-- Implementations of CIP-30 functions

enable
  :: Cip30.WalletName
  -> Array Cip30.Cip30Extension
  -> Aff (Variant (success :: Cip30Connection, apiError :: APIError))
enable wallet extensions = catchCode "enable" (Cip30.enable wallet extensions)
  toSuccess
  apiErrorMatcher

isEnabled
  :: Cip30.WalletName
  -> Aff (Variant (success :: Boolean, apiError :: APIError))
isEnabled wallet = catchCode "isEnabled" (Cip30.isEnabled wallet) toSuccess
  apiErrorMatcher

getExtensions
  :: Cip30Connection
  -> Aff (Variant (apiError :: APIError, success :: Array Cip30Extension))
getExtensions api = catchCode "getExtensions" (Cip30.getExtensions api)
  toSuccess
  apiErrorMatcher

getNetworkId
  :: Cip30Connection
  -> Aff (Variant (apiError :: APIError, success :: NetworkId))
getNetworkId api = catchCode "getNetworkId" (Cip30.getNetworkId api) toSuccess
  apiErrorMatcher

getUtxos
  :: Cip30Connection
  -> Maybe Paginate
  -> Aff (Variant (apiError :: APIError, success :: Maybe (Array Cbor)))
getUtxos api paginate = catchCode "getUtxos" (Cip30.getUtxos api paginate)
  toSuccess
  apiErrorMatcher

getCollateral
  :: Cip30Connection
  -> Cbor
  -> Aff (Variant (success :: Maybe (Array Cbor), apiError :: APIError))
getCollateral api cbor = catchCode "getCollateral"
  (Cip30.getCollateral api cbor)
  toSuccess
  apiErrorMatcher

getBalance
  :: Cip30Connection -> Aff (Variant (success :: Cbor, apiError :: APIError))
getBalance api = catchCode "getBalance" (Cip30.getBalance api) toSuccess
  apiErrorMatcher

getUsedAddresses
  :: Cip30Connection
  -> Maybe Paginate
  -> Aff (Variant (success :: Array Cbor, apiError :: APIError))
getUsedAddresses api paginate = catchCode "getUsedAddresses"
  (Cip30.getUsedAddresses api paginate)
  toSuccess
  apiErrorMatcher

getUnusedAddresses
  :: Cip30Connection
  -> Aff (Variant (success :: Array Cbor, apiError :: APIError))
getUnusedAddresses api = catchCode "getUnusedAddresses"
  (Cip30.getUnusedAddresses api)
  toSuccess
  apiErrorMatcher

getChangeAddress
  :: Cip30Connection -> Aff (Variant (success :: Cbor, apiError :: APIError))
getChangeAddress api = catchCode "getChangeAddress" (Cip30.getChangeAddress api)
  toSuccess
  apiErrorMatcher

getRewardAddresses
  :: Cip30Connection
  -> Aff (Variant (success :: Array Cbor, apiError :: APIError))
getRewardAddresses api = catchCode "getRewardAddress"
  (Cip30.getRewardAddresses api)
  toSuccess
  apiErrorMatcher

signTx
  :: Cip30Connection
  -> Cbor
  -> Boolean
  -> Aff
       ( Variant
           (apiError :: APIError, txSignError :: TxSignError, success :: Cbor)
       )
signTx api tx isPartialSign =
  catchCode "signTx" (Cip30.signTx api tx isPartialSign) toSuccess
    (apiErrorMatcher `combineErrorMatchers` txSignErrorMatcher)

signData
  :: Cip30Connection
  -> Cbor
  -> Bytes
  -> Aff
       ( Variant
           ( success :: Cip30DataSignature
           , dataSignError :: DataSignError
           , apiError :: APIError
           )
       )
signData api addr payload = catchCode "signData"
  (Cip30.signData api addr payload)
  toSuccess
  (apiErrorMatcher `combineErrorMatchers` dataSignErrorMatcher)

submitTx
  :: Cip30Connection
  -> Cbor
  -> Aff
       ( Variant
           (success :: String, apiError :: APIError, txSendError :: TxSendError)
       )
submitTx api tx = catchCode "submitTx" (Cip30.submitTx api tx) toSuccess
  (apiErrorMatcher `combineErrorMatchers` txSendErrorMatcher)

-- Error matching machinery

-- | An `ErrorMatcher` is a function that tries to match a known error based on a tag code.
newtype ErrorMatcher (row :: Row Type) = ErrorMatcher
  (Int -> String -> Maybe (Variant row))

-- | `ErrorMatcher`s can be joined: e.g `APIError` and `TxSignError` have
-- | non-intersecting error codes, so we can dispatch based on them
combineErrorMatchers
  :: forall row1 row2 row3
   . Union row1 row2 row3
  => Union row2 row1 row3
  => ErrorMatcher row1
  -> ErrorMatcher row2
  -> ErrorMatcher row3
combineErrorMatchers (ErrorMatcher f1) (ErrorMatcher f2) =
  ErrorMatcher \code info ->
    expand <$> f1 code info <|> expand <$> f2 code info

-- | Captures known error variants. Arguments:
-- |
-- | - CIP-30 method name
-- | - `Aff` action
-- | - A function that injects successful result into the row
-- | - `ErrorMatcher` that captures known errors
catchCode
  :: forall a errorRow row
   . Union errorRow (success :: a) row
  => String
  -> Aff a
  -> (a -> Variant row)
  -> (ErrorMatcher errorRow)
  -> Aff (Variant row)
catchCode functionName action handleSuccess (ErrorMatcher handleException) = do
  (action >>= handleSuccess >>> pure) `catchError` \error -> do
    errorTagInt <- liftEffect $ _getErrorTagInt error
    errorInfoString <- liftEffect $ _getErrorInfoString error
    case expand <$> handleException errorTagInt errorInfoString of
      Nothing -> do
        liftEffect $ throw $
          "CIP-30 " <> functionName
            <> ": unable to match error code with specification, code: "
            <> show errorTagInt
            <> ", info: "
            <> errorInfoString
      Just res -> pure res

toSuccess :: forall a rest. a -> Variant (success :: a | rest)
toSuccess = (inj (Proxy :: Proxy "success"))

-- | Tries to get `error.code` number, re-throws if not a CIP-30 exception
foreign import _getErrorTagInt :: Error -> Effect Int

-- | Tries to get `error.code` number, re-throws if not a CIP-30 exception
foreign import _getErrorInfoString :: Error -> Effect String

-- Error matchers. They correspond to error types in CIP-30

apiErrorMatcher :: ErrorMatcher (apiError :: APIError)
apiErrorMatcher = ErrorMatcher $
  case _ of
    (-1) -> match APIErrorInvalidRequest
    (-2) -> match APIErrorInternalError
    (-3) -> match APIErrorRefused
    (-4) -> match APIErrorAccountChange
    _ -> skip
  where
  match err info = Just $ (inj (Proxy :: Proxy "apiError")) { info, code: err }
  skip _ = Nothing

txSignErrorMatcher :: ErrorMatcher (txSignError :: TxSignError)
txSignErrorMatcher = ErrorMatcher $
  case _ of
    1 -> match TxSignErrorProofGeneration
    2 -> match TxSignErrorUserDeclined
    _ -> skip
  where
  match err info = Just $ (inj (Proxy :: Proxy "txSignError"))
    { info, code: err }
  skip _ = Nothing

dataSignErrorMatcher :: ErrorMatcher (dataSignError :: DataSignError)
dataSignErrorMatcher = ErrorMatcher $
  case _ of
    1 -> match DataSignErrorProofGeneration
    2 -> match DataSignErrorAddressNotPK
    3 -> match DataSignErrorUserDeclined
    _ -> skip
  where
  match err info = Just $ (inj (Proxy :: Proxy "dataSignError"))
    { info, code: err }
  skip _ = Nothing

txSendErrorMatcher :: ErrorMatcher (txSendError :: TxSendError)
txSendErrorMatcher = ErrorMatcher $
  case _ of
    1 -> match TxSendErrorRefused
    2 -> match TxSendErrorFailure
    _ -> skip
  where
  match err info = Just $ (inj (Proxy :: Proxy "txSendError"))
    { info, code: err }
  skip _ = Nothing
