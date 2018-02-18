---
title: About
---

Nakadi-client provides:

- Docker based test suite testing against the official Nakadi [docker
  image](https://github.com/zalando/nakadi#running-a-server).

- A rather direct translation of Nakadi's REST API to Haskell. Thus,
  if you are familiar with Nakadi's REST API, the API exposed by
  nakadi-client will feel very familiar.

- Where suitable, nakadi-client provides *additional* higher-level
  interfaces. For example, for the streaming API.

- A type-safe API for interacting with Nakadi. For example, the name
  of an event type has type `EventTypeName`, not `Text` or something
  generic.

- Integrated and configurable retry mechanism.

- Pluggable HTTP Backend for testing and mocking.
- 
- Conduit based streaming interfaces.

- Mechanism for registering callbacks for logging, token injection and
  more.

- Correct types for values like `CursorOffset` (which must be treated
  as opaque strings).
