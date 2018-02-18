# nakadi-client [![Hackage version](https://img.shields.io/hackage/v/nakadi-client.svg?label=Hackage)](https://hackage.haskell.org/package/nakadi-client) [![Stackage version](https://www.stackage.org/package/nakadi-client/badge/lts?label=Stackage)](https://www.stackage.org/package/nakadi-client) [![Build Status](https://travis-ci.org/mtesseract/nakadi-client.svg?branch=master)](https://travis-ci.org/mtesseract/nakadi-client)

### About

`nakadi-client` is a BSD2/BSD3 licensed Haskell client library for
interacting with the [Nakadi event
broker](https://zalando.github.io/nakadi/) system developed by
[Zalando](https://github.com/zalando). The streaming is built on top
of [Conduit](https://haskell-lang.org/library/conduit).

Please note that the **API is not considered stable yet**.

`nakadi-client` provides:

- Docker based test suite testing against the official Nakadi [docker
  image](https://github.com/zalando/nakadi#running-a-server) (in
  progress).

- A rather direct translation of Nakadi's REST API to Haskell. Thus,
  if you are familiar with Nakadi's REST API, the API exposed by
  `nakadi-client` will feel very familiar.

- Where suitable, `nakadi-client` provides *additional* higher-level
  interfaces.

- A type-safe API for interacting with Nakadi. For example, the name
  of an event type has type `EventTypeName`, not `Text` or something
  generic.

- Integrated and configurable retry mechanism.

- Conduit based interfaces for streaming events.

- Convenient Subscription API interface (`subscriptionSource` &
  `runSubscription`), which frees the user from any manual bookkeeping
  of the Subscription Stream ID necessary for commiting cursors.

- Mechanism for registering callbacks for logging and token injection.

- Correct types for values like `CursorOffset` (which must be treated
  as opaque strings).

- Basically each API function is exposed in two versions: One which
  requires the caller to pass in a Nakadi configuration value
  containing the information about how to connect to Nakadi and one
  which is suffixed with `R` (think: Reader monad), which expects to
  find the Nakadi configuration in the environment provided by a
  reader monad in your application's monad stack.

### Example

Example code showing how to dump a subscription:

```haskell
dumpSubscription :: (MonadLogger m, MonadNakadi IO m) => Nakadi.SubscriptionId -> m ()
dumpSubscription subscriptionId =
  Nakadi.subscriptionProcess Nothing subscriptionId processBatch

  where processBatch :: MonadLogger m => Nakadi.SubscriptionEventStreamBatch Value -> m ()
        processBatch batch =
          logInfoN (tshow batch)
```
