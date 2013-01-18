module System.USB.WMRX00 ( WMRMessage(..)
                         , WeatherForecast(..)
                         , WindDirection(..)
                         , feedWMRMessageChan
                         , wmr100source
                         ) where

import Control.Monad
import Control.Monad.IO.Class
import System.USB.HID
import qualified Data.ByteString as B
import Data.Conduit (await, yield, ($=), (=$=))
import qualified Data.Conduit as C
import Data.Conduit.Binary hiding (take)
import qualified Data.Conduit.List as L (foldM)       
import Control.Monad.IO.Class (liftIO)
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Data.DateTime
import Data.Bits
import Data.Word
import Debug.Trace
import Control.Concurrent.Chan
import Numeric

data WeatherForecast = PartlyCloudy | Rainy | Cloudy | Sunny | Snowy
                       deriving (Show, Eq, Enum)

data WindDirection = N | NNE | NE | ENE
                   | E | ESE | SE | SSE
                   | S | SSW | SW | WSW
                   | W | WNW | NW | NNW
                     deriving (Show, Eq, Enum)

data WMRMessage = DateTimeMsg       { unDateTime   :: DateTime }
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

dateTimeP :: Parser WMRMessage
dateTimeP = do char $ toEnum 0x60
               count 2 $ char $ toEnum 0
               m  <- fmap fromEnum anyChar 
               h  <- fmap fromEnum anyChar
               d  <- fmap fromEnum anyChar
               mo <- fmap fromEnum anyChar
               y  <- fmap ((+ 2000) . fromEnum) anyChar
               cs <- fmap fromEnum anyChar
               return $ DateTimeMsg $ fromGregorian (fromIntegral y) mo d h m 0

barometerP :: Parser WMRMessage
barometerP = do char $ toEnum 0x46
                anyChar
                fcast <- fmap (toEnum . (`shiftR` 4) . fromEnum) anyChar
                lsb <- fmap fromEnum anyChar
                msb <- fmap ((`shiftL` 8) . (.&. 15) . fromEnum) anyChar
                return $ BarometerMsg fcast $ fromIntegral (msb+lsb)

tempHumidityP :: Parser WMRMessage
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

anenometerP :: Parser WMRMessage
anenometerP = do char $ toEnum 0x48
                 dir  <- fmap (toEnum . fromEnum) anyChar
                 _ <- anyChar
                 gust <- fmap fromEnum anyChar
                 avgl <- fmap ((`shiftR` 4) . fromEnum) anyChar
                 avgm <- fmap ((`shiftL` 4) . (.&. 15) . fromEnum) anyChar
                 count 2 anyChar
                 let avg = avgl + avgm
                 return $ AnenometerMsg dir gust avg
                 

msgParser :: Parser WMRMessage
msgParser = do anyChar
               dateTimeP <|> barometerP <|> tempHumidityP <|> anenometerP

computeCheckSum :: B.ByteString -> Word8
computeCheckSum msg = fromInteger $ B.foldl (\a c -> a + fromIntegral (fromEnum c)) 0 msg .&. 255

usbUnwrap :: Monad m => C.GConduit B.ByteString m B.ByteString
usbUnwrap = loop
    where loop = do await >>= (maybe loop $ \packet -> case B.uncons packet of
                      Nothing       -> loop
                      Just (hd, tl) -> let len = fromIntegral hd in
                                       if len > B.length tl then loop
                                       else C.yield packet >> loop)

usbPackets :: MonadIO m => HidDevice -> C.GSource m B.ByteString
usbPackets dev = loop
  where loop = do packet <- liftIO (hidRead dev 8)
                  traceShow packet $ (C.yield packet >> loop)

processMessage :: MonadIO m => Chan WMRMessage -> B.ByteString -> m ()
processMessage msgChan str = let (msg', msg, cs) = (B.init str, B.init msg', B.last msg') in
              if computeCheckSum msg /= cs then liftIO $ putStrLn "Error: Invalid CheckSum."
              else case parse msgParser "" msg of
                     Left err -> liftIO $ print err
                     Right m  -> do liftIO $ print m
                                    liftIO $ writeChan msgChan m

data ParseState = SYNC | FIRST | READY [Word8]
splitStream :: Monad m => C.GConduit B.ByteString m B.ByteString
splitStream = awaitWithState SYNC
  where awaitWithState s = C.await >>= maybe (return ()) (wmrDecoder' s)
        wmrDecoder' s i =
          case B.uncons i of
            Nothing       -> awaitWithState s
            Just (hd, tl) -> case s of
              SYNC      -> wmrDecoder' (if hd == 0xff then FIRST    else SYNC) tl
              FIRST     -> wmrDecoder' (if hd == 0xff then READY [] else SYNC) tl
              READY acc -> if hd == 0xff then do C.yield $ B.pack $ reverse acc
                                                 wmrDecoder' SYNC tl
                           else wmrDecoder' (READY $ hd:acc) tl

-- processMessage :: MonadIO m => Chan WMRMessage -> B.ByteString -> m ()
-- processMessage msgChan str = let (msg', msg, cs) = (B.init str, B.init msg', B.last msg') in
--               if computeCheckSum msg /= cs then liftIO $ putStrLn "Error: Invalid CheckSum."
--               else case parse msgParser "" msg of
--                      Left err -> liftIO $ print err
--                      Right m  -> do liftIO $ print m
--                                     liftIO $ writeChan msgChan m

parseMessages :: Monad m => C.GConduit B.ByteString m WMRMessage
parseMessages = loop
  where loop = do res <- await
                  case res of
                    Nothing -> loop
                    Just msg -> case parse msgParser "" msg of
                      Left err -> loop
                      Right m  -> yield m >> loop

filterBadMessages :: Monad m => C.GConduit B.ByteString m B.ByteString
filterBadMessages = loop
  where loop = do s <- await
                  case s of Nothing  -> return ()
                            Just str -> let (msg', msg, cs) = (B.init str, B.init msg', B.last msg') in
                              if computeCheckSum msg /= cs then loop
                              else yield msg >> loop

wmr100source :: C.MonadResource m => C.Source m WMRMessage
wmr100source = C.bracketP (hidOpen 0x0fde 0xca01) hidClose usbPackets $= process
  where process = usbUnwrap =$= splitStream =$= filterBadMessages =$= parseMessages


feedWMRMessageChan :: MonadIO m => Chan WMRMessage -> m ()
feedWMRMessageChan chan = do dev <- liftIO $ hidOpen 0x0fde 0xca01
                             usbPackets dev C.$$ sinkUsb
                             return ()
    where sinkUsb = L.foldM iter SYNC
          iter s p = if p == B.empty then return s
                     else let (hd:tl) = B.unpack p in
                          foldM step s $ take (fromIntegral hd) tl
          step state c = 
              case state of
                SYNC  -> return $ if c == 0xff then FIRST else state
                FIRST -> return $ if c == 0xff then READY [] else SYNC
                READY acc -> if c == 0xff then do processMessage chan $ B.pack $ reverse acc
                                                  return SYNC
                             else return $ READY $ c:acc