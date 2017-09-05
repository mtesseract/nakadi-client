# nakadi-haskell [![Hackage version](https://img.shields.io/hackage/v/nakadi-haskell.svg?label=Hackage)](https://hackage.haskell.org/package/nakadi-haskell) [![Stackage version](https://www.stackage.org/package/nakadi-haskell/badge/lts?label=Stackage)](https://www.stackage.org/package/nakadi-haskell) [![Build Status](https://travis-ci.org/mtesseract/nakadi-haskell.svg?branch=master)](https://travis-ci.org/mtesseract/nakadi-haskell)

### About

`nakadi-haskell` is a Haskell client library for interacting with the
[Nakadi event broker](https://zalando.github.io/nakadi/) system
developed by [Zalando](https://github.com/zalando). The streaming is
built on top of [Conduit](https://haskell-lang.org/library/conduit).

The API is not considered stable yet.

### Example

Example using the Subscription API:

```haskell
import qualified Nakadi

processSubscription :: Nakadi.SubscriptionId -> IO ()
processSubscription subscriptionId = do
  runResourceT $ do
    (connection, source) <- Nakadi.subscriptionSource config Nothing subscriptionId
    Nakadi.runSubscription config connection $
      source .| iterMC processEvent .| Nakadi.subscriptionSink
```
