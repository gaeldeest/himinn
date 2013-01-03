{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.DateTime
import Usb.HidApi
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
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Numeric
import qualified Data.ByteString as B
import Control.Applicative ((<$>), (<*>))
import Data.Bits
import Data.Word
import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.Conduit as C
import Data.Conduit.Binary hiding (take)
import qualified Data.Conduit.List as L (foldM)
import System.IO.Unsafe

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

data WeatherForecast = PartlyCloudy | Rainy | Cloudy | Sunny | Snowy
                       deriving (Show, Eq, Enum)

data WindDirection = N | NNE | NE | ENE
                   | E | ESE | SE | SSE
                   | S | SSW | SW | WSW
                   | W | WNW | NW | NNW
                     deriving (Show, Eq, Enum)

data WmrMessage = DateTimeMsg       { unDateTime   :: DateTime }
                  | BarometerMsg    { forecast     :: WeatherForecast
                                    , pressure     :: Integer
                                    }
                  | TempHumidityMsg { sensor       :: Int
                                    , temp         :: Int
                                    , humidity     :: Int
                                    }
                  | AnenometerMsg   { direction    :: WindDirection
                                    , gustSpeed    :: Int
                                    , averageSpeed :: Int
                                    }
                    deriving (Show, Eq)

dateTimeP :: Parser WmrMessage
dateTimeP = do char $ toEnum 0x60
               count 2 $ char $ toEnum 0
               m  <- fmap fromEnum anyChar 
               h  <- fmap fromEnum anyChar
               d  <- fmap fromEnum anyChar
               mo <- fmap fromEnum anyChar
               y  <- fmap ((+ 2000) . fromEnum) anyChar
               cs <- fmap fromEnum anyChar
               return $ DateTimeMsg $ fromGregorian (fromIntegral y) mo d h m 0

barometerP :: Parser WmrMessage
barometerP = do char $ toEnum 0x46
                anyChar
                fcast <- fmap (toEnum . (`shiftR` 4) . fromEnum) anyChar
                lsb <- fmap fromEnum anyChar
                msb <- fmap ((`shiftL` 8) . (.&. 15) . fromEnum) anyChar
                return $ BarometerMsg fcast $ fromIntegral (msb+lsb)

tempHumidityP :: Parser WmrMessage
tempHumidityP = do char $ toEnum 0x42
                   sensor <- fmap ((.&. 15) . fromEnum) anyChar
                   lsb <- fmap fromEnum anyChar
                   msb <- fmap fromEnum anyChar
                   let abs  = shiftL (msb .&. 127) 8 + lsb
                       sg   = if msb .&. 128 /= 0 then -1 else 1 
                       temp =  sg*abs
                   hum <- fmap fromEnum anyChar
                   _ <- count 4 anyChar
                   return $ TempHumidityMsg sensor temp hum

anenometerP :: Parser WmrMessage
anenometerP = do char $ toEnum 0x48
                 dir  <- fmap (toEnum . fromEnum) anyChar
                 _ <- anyChar
                 gust <- fmap fromEnum anyChar
                 avgl <- fmap ((`shiftR` 4) . fromEnum) anyChar
                 avgm <- fmap ((`shiftL` 4) . (.&. 15) . fromEnum) anyChar
                 count 2 anyChar
                 let avg = avgl + avgm
                 return $ AnenometerMsg dir gust avg
                 

msgParser :: Parser WmrMessage
msgParser = do anyChar
               dateTimeP <|> barometerP <|> tempHumidityP <|> anenometerP

computeCheckSum :: B.ByteString -> Word8
computeCheckSum msg = fromInteger $ B.foldl (\a c -> a + fromIntegral (fromEnum c)) 0 msg .&. 255

dbBackend :: Chan WmrMessage -> IO ()
dbBackend msgChan = do conn <- connectPostgreSQL "user=gael dbname=wmr100"
                       feedDB conn msgChan

feedDB :: Connection -> Chan WmrMessage -> IO ()
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

process :: Chan WmrMessage -> B.ByteString -> IO ()
process msgChan str = let (msg', msg, cs) = (B.init str, B.init msg', B.last msg') in
              if computeCheckSum msg /= cs then putStrLn "Error: Invalid CheckSum."
              else case parse msgParser "" msg of
                     Left err -> print err
                     Right m  -> do print m
                                    writeChan msgChan m

usbSource :: MonadIO m => HidDevice -> C.GSource m B.ByteString
usbSource dev = loop
    where loop = do packet <- liftIO (hidRead dev 8)
                    C.yield packet >> loop

data ParseState = SYNC | FIRST | READY [Word8]
readUsb :: Chan WmrMessage -> IO ()
readUsb chan = do dev <- hidOpen 0x0fde 0xca01
                  usbSource dev C.$$ sinkUsb
                  return ()
    where sinkUsb = L.foldM iter SYNC
          iter s p = if p == B.empty then return s
                     else let (hd:tl) = B.unpack p in
                          foldM step s $ take (fromIntegral hd) tl
          step state c = 
              case state of
                SYNC  -> return $ if c == 0xff then FIRST else state
                FIRST -> return $ if c == 0xff then READY [] else SYNC
                READY acc -> if c == 0xff then do process chan $ B.pack $ reverse acc
                                                  return SYNC
                             else return $ READY $ c:acc

threadWrapper :: MVar () -> IO () -> IO ()
threadWrapper mvar action = do forkIO $ catch action (\(e::SomeException) -> putMVar mvar ())
                               return ()

main :: IO ()
main = do
  msgChan <- newChan
  mvar <- newEmptyMVar
  threadWrapper mvar $ dbBackend msgChan
  threadWrapper mvar $ readUsb msgChan
  takeMVar mvar
