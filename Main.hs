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


mkDbBackend :: Connection -> IO (WMRMessage -> IO ())
mkDbBackend c = do
  insertWindSample <- prepare c "INSERT INTO wind_samples VALUES (?,?,?,?)"
  insertTempSample <- prepare c "INSERT INTO temp_samples VALUES (?,?,?)"
  insertRHSample <- prepare c "INSERT INTO relative_humidity_samples VALUES (?,?,?)"
  insertPressureSample <- prepare c "INSERT INTO pressure_samples VALUES (?,?)"
  insertForecastSample <- prepare c "INSERT INTO forecast_samples VALUES (?,?)"
  return $ (\msg ->  do
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
           )

foreachMessage :: MonadIO m => (WMRMessage -> IO ()) -> Sink WMRMessage m ()
foreachMessage action = awaitForever onMessage
  where onMessage m = liftIO $ action m

main :: IO ()
main = withPostgreSQL "user=gael dbname=wmr100" go
    where go conn = do 
            backend <- mkDbBackend conn
            runResourceT $ wmr100source $$ foreachMessage backend
