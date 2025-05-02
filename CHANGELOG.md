# Revision history for opendht-hs

## 0.1.1.0 -- 2025-05-01

* going around an incompatibility issue around c2hs and GCC 15 (#5)
* enabling manual run for haskell-ci github action (#4)
* bootstrapped the CHANGELOG file (#3)
* fixed a memory leak (#2)

## 0.1.0.0 -- 2025-03-22

* Initial `DhtRunner` implementation:
  * `DhtRunnerM`: monad handling all pointers and state;
  * `runDhtRunnerM`: unwraps a `DhtRunnerM` action;
    * handles freeing pointers after the `DhtRunnerM` action is performed;
    * shutsdown OpenDHT gracefully running the shutdown callback and
      waiting for its termination;
  * `DhtRunnerConfig`: complete config for OpenDHT;
  * Implementation of DHT requests: `get`, `put`, `cancelPut`, `listen`, `cancelListen`.
* Minimal implementation of crypto related data types (`PublicKey`,
  `PrivateKey`, `Certificate`). Further work on this is scheduled for a
  later release;
* Complete implementation of the OpenDHT C bindings interface for:
  * Value;
  * InfoHash.
