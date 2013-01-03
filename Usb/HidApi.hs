{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, DoAndIfThenElse #-}
module Usb.HidApi ( hidInit
                  , hidOpen
                  , hidClose
                  , hidRead
                  , hidWrite
                  , HidDevice
                  ) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Exception
import Control.Monad
import Data.Typeable
import qualified Data.ByteString as B

newtype HidDevice = HidDevice (Ptr HidDevice)

data HidError = HidError deriving (Show, Typeable)

instance Exception HidError

foreign import ccall safe "hidapi/hidapi.h hid_init"
  c_hid_init :: IO CInt

hidInit :: IO ()
hidInit = do res <- c_hid_init
             if res /= 0 then throwIO HidError
             else return ()

foreign import ccall safe "hidapi/hidapi.h hid_open"
  c_hid_open :: CUShort -> CUShort -> Ptr CInt -> IO HidDevice

hidOpen :: CUShort -> CUShort -> IO HidDevice
hidOpen vendor product = do h@(HidDevice x) <- c_hid_open vendor product nullPtr
                            if x == nullPtr then throwIO HidError
                            else return h

foreign import ccall safe "hidapi/hidapi.h hid_close"
  hid_close :: HidDevice -> IO ()

hidClose :: HidDevice -> IO ()
hidClose = hid_close

foreign import ccall safe "hidapi/hidapi.h hid_write"
  c_hid_write :: HidDevice -> CString -> Int -> IO CInt

hidWrite :: HidDevice -> B.ByteString -> IO Int
hidWrite dev msg = B.useAsCStringLen msg (\(ptr, len) -> do res <- c_hid_write dev ptr len
                                                            if res == -1 then throwIO HidError
                                                            else return $ fromIntegral res)

foreign import ccall safe "hidapi/hidapi.h hid_read"
  c_hid_read :: HidDevice -> CString -> Int -> IO CInt

hidRead :: HidDevice -> Int -> IO B.ByteString
hidRead dev size = allocaBytes size $ (\ptr -> do res <- c_hid_read dev ptr size
                                                  if res == -1 then throwIO HidError
                                                  else B.packCStringLen (ptr, fromIntegral res))
