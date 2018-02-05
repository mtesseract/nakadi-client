---
title: Tutorial (Nakadi-client Library for Haskell)
---

# Tutorial for nakadi-client 0.5.x

## Importing the nakadi-client Modules

Importing the module `Network.Nakadi` brings the complete public API of the package into scope.
In order to prevent namespace clashes it is recommended practice to import this module qualified. If you decide to do so, it is a good idea to additionally import `Network.Nakadi.Prelude` *unqualified* — this brings into scope names like `MonadNakadi`, which hardly need to be imported qualified.

That being said, start with the following imports:

```haskell
import qualified Network.Nakadi as Nakadi
import           Network.Nakadi.Prelude
```

## Creating the Nakadi Configuration

In order to use Nakadi, you need to create a configuration object. The function of choice for creating such a configuration is the following:

```haskell
newConfigIO :: Request -> ConfigIO
```

The only parameter required by this function is a HTTP `Request`, which serves as the so-called *request template*. Every HTTP request executed by nakadi-client is produced by modifying this request template. This is the place where you specify the Nakadi endpoint to use. For example:

The value it returns is of type `ConfigIO`, which denotes a Nakadi configuration whose *Nakadi base monad* is `IO`. Generally, the Nakadi base monad is the monad in which the HTTP backend code and any registered user callback runs.

Even though you can easily get going with `newConfigIO`, it is not recommended to use this function in applications which need to follow certain resilience patterns: In an application doing a lot of HTTP calls, it is often desired to keep certain types of HTTP calls isolated from each other. 


<!-- We will see later which other functions for creating configurations exist in case more flexibility or resilience is desired. -->

<!-- ```haskell
newConfigWithDedicatedManager :: (MonadIO b, MonadMask b, MonadIO m)
                              => ManagerSettings -> Request -> m (Config b)
``` -->

# Advanced Usage


In addition to a `Request`, this function also requires `ManagerSettings` to be specified. With these settings a new dedicated HTTP Manager will be created exclusively for use by nakadi-client. The configuration produced by the function `newConfigIO` on the other hand uses some global default HTTP manager. Using a dedicated manager with specified settings is a common resilience pattern.

In any case, these functions produce a value of type `Config b`? The monad `b` acts as a *base monad* within nakadi-client. It denotes the monad in which user-provided callbacks are running. For a start, taking `b` to mean `IO` is appropriate. For more complicated applications involving Monad transformer stacks it might be very useful to ... -->

## Calling Nakadi

Most of the functions contained in nakadi-client return values in some monad `m` satisfying the constraint `MonadNakadi b m`. The primary examples for `m` are built using the `NakadiT b` monad transformer. Again, `b` denotes some fixed base monad in the background.

Given an action wrapped within the `NakadiT` transformer, the following function is used for *running* it:

```haskell
runNakadiT :: Config b -> NakadiT b m a -> m a
```

Thus, a simple example for listing all registered events types can be written as follows:

```
dumpEventTypes :: Nakadi.ConfigIO -> LoggingT IO ()
dumpEventTypes config = Nakadi.runNakadiT config $ do
  eventTypes <- Nakadi.eventTypesList
  forM_ eventTypes $ \ eventType ->
    logInfoN $ tshow (eventType^.L.name)
```

Here, we are given a configuration of type `ConfigIO` and pass this to
`runNakadiT` as first argument. Within the provided monadic action
(second argument to `runNakadiT`) we have full access to the Nakadi
API. The action `eventTypesList` for example has the following type
signature

```
eventTypesList :: MonadNakadi b m => m [EventType]
```

In other words, under the `MonadNakadi` typeclass constraint, this
action produces a list of event types (i.e., values of type type
`EventType`).

## Consuming a Subscription

Consuming a Nakadi subscription can easily be done using the
`subscriptionProcess` action. Let us consider an example:

```
dumpSubscription :: (MonadLogger m, MonadNakadi IO m, MonadMask m) => Nakadi.SubscriptionId -> m ()
dumpSubscription subscriptionId =
  Nakadi.subscriptionProcess Nothing subscriptionId processBatch

  where processBatch :: MonadLogger m => Nakadi.SubscriptionEventStreamBatch Value -> m ()
        processBatch batch =
          logInfoN (tshow batch)
```
