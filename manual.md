---
title: Nakadi-Client Manual
---

# Foreword

This is the manual for the nakadi-client package for Haskell.
Nakadi-client is a free software Haskell client library for
interacting with the [Nakadi event
broker](https://zalando.github.io/nakadi/) system developed by
[Zalando](https://github.com/zalando). The API exposed by
nakadi-client is closely modelled after the official Nakadi API.

# Type Foundation

Nakadi-client exposes the typeclass `MonadNakadi` which defines
monads, in which Nakadi can be called. More precisely, `MonadNakadi`
is a multi-parameter typeclass of the form `MonadNakadi b m`, where
`b` is a fixed base monad. The concept of a base monad is required
here to enable the user of nakadi-client to register callbacks that do
not necessarily have to live in a fixed monad like `IO`. Instead, the
user can decide for an application-specific base monad, which will
then be the monad in which the callbacks run. For the special case of
`MonadNakadi IO` nakadi-client exposes the constraint type synonym
`MonadNakadiIO`.

A superclass of the `MonadNakadi` typeclass is `MonadNakadiBase`.
While `MonadNakadi` is essentially a specialized reader monad where
the stored environment is a Nakadi configuration object (parameterized
by the provided base monad `b`), `MonadNakadiBase` is essentially a
specialized version of `MonadBase`, which we use for lifting
user-provided callbacks contained in the configuration object.

As long as the user sticks to some standard monad transformer stacks,
there is usually no need to implement these instances, as the required
instances already exist. What follows is a more in-depth description
of these monads and their semantics.

## The Monad `MonadNakadiBase`

Let us start with `MonadNakadiBase`. It is defined as follows:

```haskell
class (Monad b, Monad m) => MonadNakadiBase b m where
  nakadiLiftBase :: b a -> m a
```

Nakadi-client already provides the required base instances for the
monads `IO`, `ReaderT r m` and `LoggingT (ReaderT r m)`:

```haskell
instance {-# OVERLAPPING #-} MonadNakadiBase IO IO where
  nakadiLiftBase = identity

instance {-# OVERLAPPING #-} Monad m => MonadNakadiBase (ReaderT r m) (ReaderT r m) where
  nakadiLiftBase = identity

instance {-# OVERLAPPING #-} Monad m => MonadNakadiBase (LoggingT (ReaderT r m)) (LoggingT (ReaderT r m)) where
  nakadiLiftBase = identity
```

Furthermore, there exist instances implementing `MonadNakadiBase` on
top of the monad transforms `ReaderT`, `WriterT`, `LoggingT`,
`NoLoggingT`, `ResourceT` and `StateT`.

Therefore, for one of the above base monads `b`, `MonadNakadiBase b`
is automatically available for any monad transformer stack `m` on top
of `b` being constructed in terms of the monad transformers `ReaderT`,
`WriterT`, `LoggingT`, `NoLoggingT`, `ResourceT` and `StateT`.

For using custom base monads with `MonadNakadiBase`, see [Advanced
usage].

## The Monad `MonadNakadi`

The typeclass `MonadNakadi` is defined as follows:

```haskell
class (MonadNakadiBase b m, MonadThrow b, MonadMask b, MonadThrow m, MonadCatch m)
   => MonadNakadi b m | m -> b where
  -- {-# MINIMAL (nakadiAsk | nakadiReader), nakadiLocal #-}
  nakadiAsk :: m (Config b)
  nakadiLocal :: (Config b -> Config b) -> m a -> m a
  nakadiReader :: (Config b -> a) -> m a
```

Clearly, this is just a specialized version or `MonadReader`.

This typeclass is accompanied by the type

```haskell
newtype NakadiT b m a = NakadiT { _runNakadiT :: Config b -> m a }
```

which is closely modelled after the `ReaderT` transformer. Here, `b`
denotes the base monad. The type `NakadiT` defines a monad transformer
and inherits implementations of `MonadThrow`, `MonadCatch`,
`MonadMask`, `MonadIO`, `MonadBase`, `MonadReader`, `MonadLogger`,
`MonadState`, `MonadUnliftIO`, `MonadTransControl`,
`MonadBaseControl`.

# Nakadi API

