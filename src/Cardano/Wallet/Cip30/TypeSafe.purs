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
  , PaginateError
  , TxSendErrorTag(TxSendErrorRefused, TxSendErrorFailure)
  , TxSendError
  , TxSignErrorTag(TxSignErrorProofGeneration, TxSignErrorUserDeclined)
  , TxSignError
  , signTx
  , getExtensions
  ) where

import Prelude

import Cardano.Wallet.Cip30 (Bytes, Cbor, Cip30Connection, Cip30DataSignature, Cip30Extension, NetworkId, Paginate, WalletName)
import Cardano.Wallet.Cip30 (enable, getApiVersion, getName, getIcon, isEnabled, isWalletAvailable, getBalance, getChangeAddress, getCollateral, getNetworkId, getRewardAddresses, getUnusedAddresses, getUsedAddresses, getUtxos, signTx, signData, submitTx, getExtensions, getAvailableWallets) as Cip30
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

type PaginateError = { maxSize :: Int }

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

signTx :: Cip30Connection -> Cbor -> Boolean -> Aff (Variant (apiError :: APIError, txSignError :: TxSignError, success :: Cbor))
signTx api tx isPartialSign =
  catchCode "signTx" (Cip30.signTx api tx isPartialSign) toSuccess (apiErrorMatcher `combineErrorMatchers` txSignErrorMatcher)

getExtensions
  :: Cip30Connection
  -> Aff (Variant (apiError :: APIError, success :: Array Cip30Extension))
getExtensions api = catchCode "getExtensions" (Cip30.getExtensions api) toSuccess apiErrorMatcher

getNetworkId
  :: Cip30Connection
  -> Aff (Variant (apiError :: APIError, success :: NetworkId))
getNetworkId api = catchCode "getNetworkId" (Cip30.getNetworkId api) toSuccess apiErrorMatcher

toSuccess :: forall a rest. a -> Variant (success :: a | rest)
toSuccess = (inj (Proxy :: Proxy "success"))

txSignErrorMatcher :: ErrorMatcher (txSignError :: TxSignError)
txSignErrorMatcher = ErrorMatcher $
  case _ of
    1 -> match TxSignErrorProofGeneration
    2 -> match TxSignErrorUserDeclined
    _ -> skip
  where
  match err info = Just $ (inj (Proxy :: Proxy "txSignError")) { info, code: err }
  skip _ = Nothing

-- | An `ErrorMatcher` is a function that tries to match a known error based on a tag code.
newtype ErrorMatcher (row :: Row Type) = ErrorMatcher (Int -> String -> Maybe (Variant row))

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

-- | `APIError` error code matcher from CIP-30 spec
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
          "CIP-30 " <> functionName <> ": unable to match error code with specification, code: "
            <> show errorTagInt
            <> ", info: "
            <> errorInfoString
      Just res -> pure res

-- | Tries to get `error.code` number, re-throws if not a CIP-30 exception
foreign import _getErrorTagInt :: Error -> Effect Int

-- | Tries to get `error.code` number, re-throws if not a CIP-30 exception
foreign import _getErrorInfoString :: Error -> Effect String
