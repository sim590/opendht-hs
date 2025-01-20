
module Main where

import qualified Data.ByteString as BS
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
          putStrLn $ ">> data: " <> take 50 (map BSI.w2c (BS.unpack d))

doneCb :: MVar () -> Bool -> () -> IO ()
doneCb mv success _ = do
  when success   $ putStrLn "Succès!"
  unless success $ putStrLn "Échec!"
  putMVar mv ()

main :: IO ()
-- main = putStrLn "toto"
main = do
  mv <- newEmptyMVar
  runDhtRunnerM $ do
    DhtRunner.run 0
    DhtRunner.bootstrap "bootstrap.ring.cx" "4222"
    h <- lift randomInfoHash
    liftIO $ putStrLn $ "Mon hash: " <> show h
    DhtRunner.get h getCb (doneCb mv) ()
    liftIO $ takeMVar mv

--  vim: set sts=2 ts=2 sw=2 tw=120 et :

