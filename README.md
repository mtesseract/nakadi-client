# nakadi-haskell

`nakadi-haskell` is a Haskell client library for interacting with the
[Nakadi event broker](https://zalando.github.io/nakadi/) system
developed by [Zalando](https://github.com/zalando).

The API is not considered stable yet.

Example using Subscription API:

```haskell
  runResourceT $ do
    (connection, stream) <- subscriptionSource config Nothing subscriptionId
    runSubscription config connection $
      source
      .| C.iterM processEvent
      .| subscriptionSink
```
