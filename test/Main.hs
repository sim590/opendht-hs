
module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent.MVar

import OpenDHT.Value
import OpenDHT.InfoHash
import OpenDHT.DhtRunner ( runDhtRunnerM
                         )
import qualified OpenDHT.DhtRunner as DhtRunner

getCb :: Value -> () -> IO Bool
getCb (InputValue _ _) _              = undefined
getCb (StoredValue d i _ rId utype) _ = liftIO cb >> return True
  where cb = do
          putStrLn "Valeur retrouvée!"
          putStrLn $ ">> data: "         <> take 50 (map BSI.w2c (BS.unpack d))
          putStrLn $ ">> value id: "     <> show i
          putStrLn $ ">> recipient id: " <> show rId
          putStrLn $ ">> user type: "    <> show utype

doneCb :: MVar () -> Bool -> () -> IO ()
doneCb mv success _ = do
  when success   $ putStrLn "Succès!"
  unless success $ putStrLn "Échec!"
  putMVar mv ()

main :: IO ()
main = do
  mv <- newEmptyMVar
  runDhtRunnerM $ do
    DhtRunner.run 0
    DhtRunner.bootstrap "bootstrap.ring.cx" "4222"
    h <- lift randomInfoHash
    liftIO $ putStrLn $ "Mon hash: " <> show h
    let v = InputValue { _valueData     = BSC.pack "toto"
                       , _valueUserType = "mytype"
                       }
    DhtRunner.put h v (doneCb mv) () False
    liftIO $ takeMVar mv
    DhtRunner.get h getCb (doneCb mv) ()
    liftIO $ takeMVar mv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

