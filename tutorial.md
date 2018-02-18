---
title: Tutorial
---

# Tutorial for nakadi-client 0.5.x (Work in Progress)

This tutorial provides an quick & easy introduction for using the
Nakadi-client package for Haskell. For more in-depth information, have
a look at the [API
reference](https://hackage.haskell.org/package/nakadi-client).

## Importing the nakadi-client Modules

Importing the module `Network.Nakadi` brings the complete public API
of the package into scope. In order to prevent namespace clashes it is
recommended practice to import this module qualified. If you decide to
do so, it is a good idea to additionally import
`Network.Nakadi.Prelude` *unqualified* — this brings into scope names
like `MonadNakadi`, which hardly need to be imported qualified.

That being said, start with the following imports:

```haskell
import qualified Network.Nakadi as Nakadi
import           Network.Nakadi.Prelude
```

## Creating the Nakadi Configuration

In order to use Nakadi, you need to create a configuration object. The
function of choice for creating such a configuration is the following:

```haskell
newConfigIO :: Request -> ConfigIO
```

The only parameter required by this function is a HTTP `Request`,
which serves as the so-called *request template*. Every HTTP request
executed by nakadi-client is produced by modifying this request
template. This is the place where you specify the Nakadi endpoint to
use. For example:

The value it returns is of type `ConfigIO`, which denotes a Nakadi
configuration whose *Nakadi base monad* is `IO`. Generally, the Nakadi
base monad is the monad in which the HTTP backend code and any
registered user callback runs.

Even though you can easily get going with `newConfigIO`, it is not
recommended to use this function in applications which need to follow
certain resilience patterns: In an application doing a lot of HTTP
calls, it is often desired to keep certain types of HTTP calls
isolated from each other.

## Calling Nakadi

Most of the functions contained in nakadi-client return values in some
monad `m` satisfying the constraint `MonadNakadi b m`. This constraint
is automatically satisfied when you use `runNakadiT` for producing
actions living within the `NakadiT` monad transformer.

A simple example for listing all registered events types can be
written as follows:

```
dumpEventTypes :: Nakadi.ConfigIO -> LoggingT IO ()
dumpEventTypes config = Nakadi.runNakadiT config $ do
  eventTypes <- Nakadi.eventTypesList
  forM_ eventTypes $ \ eventType ->
    logInfoN $ tshow (eventType^.L.name)
```

Here we are given a configuration of type `ConfigIO` and provide this
to `runNakadiT` as the first argument. Within the provided monadic
action (second argument to `runNakadiT`) we have full access to the
Nakadi API. Not that `eventTypesList` has the following type
signature:

```
eventTypesList :: MonadNakadi b m => m [EventType]
```

## Consuming a Subscription

One of the most important use cases for nakadi-client is the
consumption of a subscription. Consuming a Nakadi subscription can
easily be done using the `subscriptionProcess` function. Let us
consider an example:

```
dumpSubscription :: (MonadLogger m, MonadNakadi IO m, MonadMask m) => Nakadi.SubscriptionId -> m ()
dumpSubscription subscriptionId =
  Nakadi.subscriptionProcess Nothing subscriptionId processBatch

  where processBatch :: MonadLogger m => Nakadi.SubscriptionEventStreamBatch Value -> m ()
        processBatch batch =
          logInfoN (tshow batch)
```

Here we are calling the `subscriptionProcess` function providing only no
special consumption parameters (first argument is `Nothing`), which
means that the default consumption parameteres
(`defaultConsumeParameters`) are used. Furthermore, we provide the
subcriptionId and a function which processes event batches.
