# purescript-cip30-typesafe

An extension of [`purescript-cip30`](https://github.com/mlabs-haskell/purescript-cip30) with PureScript wrapper types for CIP-30 errors.

Simply put, it lets you dispatch on known error values by using `Variant`:

```purescript
-- purescript-cip30 ignores the errors:
signTx :: Cip30Connection -> Cbor -> Boolean -> Aff Cbor

-- purescript-cip30-typesafe catches and lifts exceptions into the PureScript world as values:
signTx
  :: Cip30Connection
  -> Cbor
  -> Boolean
  -> Aff
       ( Variant
           (apiError :: APIError, txSignError :: TxSignError, success :: Cbor)
       )
```

This interface forces the user to explicitly handle all known CIP-30 errors. Note that the presence of `Aff` still implies possibility of exceptions.
