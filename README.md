
## opendht-hs

Haskell bindings for [OpenDHT](https://github.com/savoirfairelinux/opendht)
(based on opendht-c, the C bindings for OpenDHT) exposing only pure Haskell data
types.

This library defines a monad taking care of all pointers used to interact with
opendht-c.

### Example

```haskell
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent

import OpenDHT.Value
import OpenDHT.InfoHash
import OpenDHT.PublicKey
import OpenDHT.DhtRunner ( runDhtRunnerM
                         )
import qualified OpenDHT.DhtRunner as DhtRunner

getCb :: Value -> IO Bool
getCb v = valueCb v False

doneCb :: MVar () -> Bool -> IO ()
doneCb mv success = do
  let status
        | success   = "Success!"
        | otherwise = "Fail!"
  putStrLn status
  putMVar mv ()

valueCb :: Value -> Bool -> IO Bool
valueCb (MetaValue {}) _                       = undefined
valueCb (InputValue {}) _                      = undefined
valueCb (StoredValue _ _ (PublicKey {}) _ _) _ = undefined
valueCb (StoredValue d i (ExportedKey o) rId utype) expired = liftIO cb >> return True
  where
    ownerStr = if null o then "none" else take 50 o
    dString  = map BSI.w2c (BS.unpack d)
    cb = do
      putStrLn $ "Value "            <> if expired then "expired!" else "received!"
      putStrLn $ ">> data: "         <> take 50 dString
      putStrLn $ ">> value id: "     <> show i
      putStrLn $ ">> value owner: "  <> ownerStr
      putStrLn $ ">> recipient id: " <> show rId
      putStrLn $ ">> user type: "    <> show utype

shutdownCb :: MVar () -> IO ()
shutdownCb mv = do
  putStrLn "Shutting down!"
  takeMVar mv

main :: IO ()
main = do
  mv         <- newEmptyMVar
  shutdownMV <- newMVar ()
  runDhtRunnerM (shutdownCb shutdownMV) shutdownMV $ do
    DhtRunner.run 0
    DhtRunner.bootstrap "bootstrap.ring.cx" "4222"

    h <- lift randomInfoHash
    let
      dataStr = "my data"
      v       = InputValue { _valueData     = BSC.pack dataStr
                           , _valueUserType = "mytype"
                           }
    liftIO $ putStrLn $ "Putting value " <> show dataStr
    void $ DhtRunner.put h v (doneCb mv) False
    liftIO $ takeMVar mv

    liftIO $ putStrLn "Getting back the value..."
    DhtRunner.get h getCb (doneCb mv)
    liftIO $ takeMVar mv
```

This would yield the following text on `stdout`:

```
Putting value "my data"
Success!
Getting back the value...
Value received!
>> data: my data
>> value id: 11921635044826781238
>> value owner: none
>> recipient id: 0000000000000000000000000000000000000000
>> user type: "mytype"
Success!
Shutting down!
```

### Using opendht-hs

Make sure to pass `-threaded` to GHC because this library makes use of
threading.

### Dependencies

OpenDHT's C bindings.

### Author(s)

Simon Désaulniers (sim.desaulniers@gmail.com)

<!-- vim: set sts=2 ts=2 sw=2 tw=0 et :-->

