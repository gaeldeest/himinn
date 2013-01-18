{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.DateTime
import System.USB.WMRX00
import System.Process
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Prelude hiding (catch)
import Control.Exception (catch, SomeException)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.Posix.IO

import Control.Monad.Trans.Resource
import Data.Conduit (Sink, awaitForever, ($$))
import qualified Data.ByteString as B
import Control.Applicative ((<$>), (<*>))
import Data.Bits
import Data.Word
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL


dbBackend :: Chan WMRMessage -> IO ()
dbBackend msgChan = do conn <- connectPostgreSQL "user=gael dbname=wmr100"
                       feedDB conn msgChan

feedDB :: Connection -> Chan WMRMessage -> IO ()
feedDB c chan = do
  insertWindSample <- prepare c "INSERT INTO wind_samples VALUES (?,?,?,?)"
  insertTempSample <- prepare c "INSERT INTO temp_samples VALUES (?,?,?)"
  insertRHSample <- prepare c "INSERT INTO relative_humidity_samples VALUES (?,?,?)"
  insertPressureSample <- prepare c "INSERT INTO pressure_samples VALUES (?,?)"
  insertForecastSample <- prepare c "INSERT INTO forecast_samples VALUES (?,?)"
  forever $ do
    msg <- readChan chan
    at <- fmap toSql getCurrentTime
    print msg
    case msg of
      BarometerMsg fcast pressure -> do
                  execute insertPressureSample [ at, toSql pressure ]
                  execute insertForecastSample [ at, toSql $ fromEnum fcast ]
                  return ()
      TempHumidityMsg sensor temp humidity -> do
                  execute insertTempSample [ at, toSql sensor, toSql temp ]
                  execute insertRHSample [ at, toSql sensor, toSql humidity ]
                  return ()
      AnenometerMsg dir gust avg -> do
                  execute insertWindSample [ at
                                           , toSql $ fromEnum dir
                                           , toSql gust
                                           , toSql avg
                                           ]
                  return ()
      _ -> return ()
    commit c


threadWrapper :: MVar SomeException -> IO () -> IO ()
threadWrapper mvar action = do forkIO $ catch action (\(e::SomeException) -> putMVar mvar e)
                               return ()

justReadMessages :: MonadIO m => (WMRMessage -> IO ()) -> Sink WMRMessage m ()
justReadMessages action = awaitForever onMessage
  where onMessage m = liftIO $ action m

main :: IO ()
main = runResourceT $ wmr100source $$ justReadMessages print
