module Test.Main where

import Prelude

import Cardano.Wallet.Cip30.TypeSafe
  ( APIErrorTag
      ( APIErrorInvalidRequest
      , APIErrorInternalError
      , APIErrorRefused
      , APIErrorAccountChange
      )
  , DataSignErrorTag
      ( DataSignErrorProofGeneration
      , DataSignErrorAddressNotPK
      , DataSignErrorUserDeclined
      )
  , TxSendErrorTag(TxSendErrorRefused, TxSendErrorFailure)
  , TxSignErrorTag(TxSignErrorProofGeneration, TxSignErrorUserDeclined)
  , getBalance
  , getUsedAddresses
  , getUtxos
  , signData
  , signTx
  , submitTx
  )
import Data.Bifunctor (lmap)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Nothing))
import Data.Variant (inj)
import Effect (Effect)
import Effect.Aff (launchAff_, try)
import Effect.Exception (message, throw)
import Test.Inject (mkApi, throwingApi)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ] do
    describe "TypeSafe API" do
      it "passes the exception through if it is unknown" do
        try (getBalance $ mkApi (throw "hi"))
          >>= lmap message >>> shouldEqual
            ( Left
                "CIP-30 getBalance: unable to match error with specification: hi"
            )
      it "passes the exception through if no info string is specified" do
        try (getBalance $ throwingApi { code: -1 })
          >>= lmap message >>> shouldEqual
            ( Left
                "CIP-30 getBalance: unable to match error with specification: undefined"
            )
      it "passes the exception through if error code is unknown" do
        try (getBalance $ throwingApi { code: -5, info: "hiii" })
          >>= lmap message >>> shouldEqual
            ( Left
                "CIP-30 getBalance: unable to match error with specification: undefined"
            )
      describe "catching APIError variants" do
        it "catches APIErrorInvalidRequest" do
          (getBalance $ throwingApi { info: "hiii", code: -1 })
            >>= shouldEqual
              ( inj (Proxy :: Proxy "apiError")
                  { code: APIErrorInvalidRequest, info: "hiii" }
              )
        it "catches APIErrorInternalError" do
          (getBalance $ throwingApi { info: "hiii", code: -2 })
            >>= shouldEqual
              ( inj (Proxy :: Proxy "apiError")
                  { code: APIErrorInternalError, info: "hiii" }
              )
        it "catches APIErrorRefused" do
          (getBalance $ throwingApi { info: "hiii", code: -3 })
            >>= shouldEqual
              ( inj (Proxy :: Proxy "apiError")
                  { code: APIErrorRefused, info: "hiii" }
              )
        it "catches APIErrorAccountChange" do
          (getBalance $ throwingApi { info: "hiii", code: -4 })
            >>= shouldEqual
              ( inj (Proxy :: Proxy "apiError")
                  { code: APIErrorAccountChange, info: "hiii" }
              )
        it "catches APIErrorAccountChange #2" do
          -- same with signData
          (signData (throwingApi { info: "hiii", code: -4 })) "" ""
            >>= shouldEqual
              ( inj (Proxy :: Proxy "apiError")
                  { code: APIErrorAccountChange, info: "hiii" }
              )
      describe "catching DataSignError variants" do
        it "catches DataSignErrorProofGeneration" do
          (signData (throwingApi { info: "hiii", code: 1 }) "" "")
            >>= shouldEqual
              ( inj (Proxy :: Proxy "dataSignError")
                  { code: DataSignErrorProofGeneration, info: "hiii" }
              )
        it "catches DataSignErrorAddressNotPK" do
          (signData (throwingApi { info: "hiii", code: 2 }) "" "")
            >>= shouldEqual
              ( inj (Proxy :: Proxy "dataSignError")
                  { code: DataSignErrorAddressNotPK, info: "hiii" }
              )
        it "catches DataSignErrorUserDeclined" do
          (signData (throwingApi { info: "hiii", code: 3 }) "" "")
            >>= shouldEqual
              ( inj (Proxy :: Proxy "dataSignError")
                  { code: DataSignErrorUserDeclined, info: "hiii" }
              )
      describe "catching TxSignError variants" do
        it "catches TxSignErrorProofGeneration" do
          (signTx (throwingApi { info: "hiii", code: 1 }) "" false)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSignError")
                  { code: TxSignErrorProofGeneration, info: "hiii" }
              )
        it "catches TxSignErrorUserDeclined" do
          (signTx (throwingApi { info: "hiii", code: 2 }) "" false)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSignError")
                  { code: TxSignErrorUserDeclined, info: "hiii" }
              )
      describe "catching TxSendError variants" do
        it "catches TxSendErrorRefused" do
          (submitTx (throwingApi { info: "hiii", code: 1 }) "")
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSendError")
                  { code: TxSendErrorRefused, info: "hiii" }
              )
        it "catches TxSendErrorFailure" do
          (submitTx (throwingApi { info: "hiii", code: 2 }) "")
            >>= shouldEqual
              ( inj (Proxy :: Proxy "txSendError")
                  { code: TxSendErrorFailure, info: "hiii" }
              )
      describe "catching PaginateError" do
        it "catches PaginateError from getUtxos" do
          (getUtxos (throwingApi { maxSize: 1 }) Nothing Nothing)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "paginateError")
                  { maxSize: 1 }
              )
        it "catches PaginateError from getUsedAddresses" do
          (getUsedAddresses (throwingApi { maxSize: 1 }) Nothing)
            >>= shouldEqual
              ( inj (Proxy :: Proxy "paginateError")
                  { maxSize: 1 }
              )
