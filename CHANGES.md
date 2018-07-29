# Nakadi-Client Change Log

## v0.6.0.0

* Nakadi *business events* are supported.

* The modelling of the subscription API has been simplified by by differentiating between `SubscriptionRequest` (before creation) and `Subscription` (after creation).

* An experimental API for creating a Conduit event source for a given subscription is included, allowing simulating the old low-level API using the Subscription API.

* A new function `withSubscription` is included, providing an interface for creating a subscription and automatically passing its subscription ID to some user-provided action.

* A new function `withTemporarySubscription` is included, which is very similar to `withSubscription`, but with the crucicial difference that the subscription will be automatically deleted after the user-provided action has terminated.

* Support for the new `show_time_lag` field when retrieving subscription statistics has been added.

* An experimental `MonadNakadi` instance for the `IO` monad using a global Nakadi configuration has been added. The new module `Network.Nakadi.Unsafe.IO` exposes functionality for accessing this global Nakadi configuration. The new instance for `IO` allows e.g. the evaluation of Nakadi calls interactively in GHCi without the need to run any monad transformers.

* A new convenience function `configFromEnv` is exposed, which allows creating a Nakadi configuration with the Nakadi service URL being derived automatically from the environment variable `NAKADI_URL`.
