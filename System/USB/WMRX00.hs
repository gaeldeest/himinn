module System.USB.WMRX00 ( WMRMessage(..)
                         , WeatherForecast(..)
                         , WindDirection(..)
                         , wmr100source
                         ) where

import Prelude hiding (take, init, last, length, filter, foldl)
import Control.Monad
import Control.Monad.IO.Class
import System.USB.HID
import Data.ByteString (ByteString, take, init, last, length, uncons, pack, foldl)
import Data.Conduit (await, yield, bracketP, ($=), (=$=), GSource, Source, GInfConduit, MonadResource)
import Data.Conduit.List (mapMaybe, concatMapAccum, filter)
import qualified Data.Conduit as C
import Data.Conduit.Binary hiding (take)
import Control.Monad.IO.Class (liftIO)
import Text.Parsec.Prim hiding (uncons)
import Text.Parsec.Char
import Text.Parsec.ByteString
import Text.Parsec.Combinator
import Data.DateTime
import Data.Bits
import Data.Word
import Debug.Trace
import Control.Concurrent.Chan
import Numeric

data WeatherForecast = PartlyCloudy | Rainy | Cloudy | Sunny | ClearNight | Snowy | PartlyCloudyNight
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

-- | Produces a stream of (normally) eight bytes 'ByteString' packets.
usbPackets :: MonadIO m => HidDevice -> GSource m ByteString
usbPackets dev = loop
  where loop = do packet <- liftIO (hidRead dev 8)
                  yield packet >> loop

-- | Unwraps packets, stripping non-data bytes.
-- The first byte of a USB packet indicates the number of data bytes it contains.
-- This conduit reads this value and extracts the right number of bytes from the
-- remaining string.
unwrap :: Monad m => GInfConduit ByteString m ByteString
unwrap = mapMaybe unwrapSingle
    where unwrapSingle msg =
              case uncons msg of
                Nothing       -> Nothing
                Just (hd, tl) -> let len = fromIntegral hd in
                                 if len > length tl then Nothing
                                 else Just $ take len tl

-- | Used internally to keep track of sync state.
data ParseState = SYNC | FIRST | READY [Word8]

-- | Extracts messages from a 'ByteString' stream.
-- In (unwrapped) USB data, messages are separated by two 0xff bytes.
-- This conduit splits the incoming stream in messages.
splitStream :: Monad m => GInfConduit ByteString m ByteString
splitStream = concatMapAccum splitter SYNC
  where splitter packet state =
          case uncons packet of
            Nothing       -> (SYNC, [])
            Just (hd, tl) -> foldl iter (state, []) packet
        iter (parseState, acc) c =
          case parseState of
            SYNC     -> if c == 0xff then (FIRST,    acc) else (SYNC, acc)
            FIRST    -> if c == 0xff then (READY [], acc) else (SYNC, acc)
            READY ws -> if c == 0xff then (FIRST, acc ++ [pack ws])
                        else (READY $ ws++[c], acc) 

-- | Simple checksum computation for WMR100.
-- Checksum computation is simply an addition modulo 256.
computeCheckSum :: ByteString -> Word8
computeCheckSum msg = fromInteger $ foldl (\a c -> a + fromIntegral (fromEnum c)) 0 msg .&. 255

-- | Performs a checksum test on each message, dropping corrupt ones.
filterBadMessages :: Monad m => GInfConduit ByteString m ByteString
filterBadMessages = filter validChecksum
    where validChecksum m = let (msg', msg, cs) = (init m, init msg', last msg') in
                            computeCheckSum msg == cs

-- | Parses a stream of 'ByteString's into a stream of 'WMRMessage's.
parseMessages :: Monad m => GInfConduit ByteString m WMRMessage
parseMessages = mapMaybe parseSingle
    where parseSingle str = case parse msgParser "" str of
                              Left err  -> Nothing
                              Right msg -> Just msg

-- | Connects to WMR100 weather station on USB and produces a stream of messages.
wmr100source :: MonadResource m => Source m WMRMessage
wmr100source = bracketP (hidOpen 0x0fde 0xca01) hidClose usbPackets $= process
  where process = unwrap =$= splitStream =$= filterBadMessages =$= parseMessages
