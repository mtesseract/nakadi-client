---
title: FAQ
---

### How do you use tokens?

Install a so-called *request modifier* in the Nakadi configuration
using the following function:

```haskell
setRequestModifier :: (Request -> m Request) -> Config m -> Config m
```

This request modifier is then run for every HTTP request initiated by
nakadi-client and can inject token headers into the request.

### Can I simply put a Nakadi configuration in my own Reader monad environment?

Yes. If your application's monad stack contains the `ReaderT`
transformer, you can simply use it for providing the Nakadi
configuration to nakadi-client instead of adding the `NakadiT` monad
transformer on top of your existing monad transformer stack.

All that is required is to implement the `HasNakadiConfig` typeclass
for your `ReaderT`'s environment.

Let us consider an example. Assume you have the following data type:

```haskell
data MyEnv = MyEnv { _dbConnection :: DBConnection }
```

we simply add a field for the Nakadi configuration:
```haskell
data MyEnv = MyEnv { dbConnection   :: DBConnection
                   , myNakadiConfig :: Nakadi.ConfigIO }
```

Here we have simply used `IO` as the Nakadi base monad, you can of
course use some other Nakadi base monad — say `AppM` — using the
following code
```haskell
data MyEnv = MyEnv { dbConnection   :: DBConnection
                   , myNakadiConfig :: Nakadi.Config AppM }
```
Now we can implement the `HasNakadiConfig` typeclass by saying
```haskell
instance HasNakadiConfig AppM MyEnv where
  nakadiConfig = myNakadiConfig
```

There is a `MonadNakadi` instance for `ReaderT r` provided that the
constraint `HasNakadiConfig r` is satisfied.
