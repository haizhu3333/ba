module Main where

import Foreign
import Foreign.C

foreign import ccall unsafe "ErrorMessage" dds_ErrorMessage :: CInt -> Ptr CChar -> IO ()

main :: IO ()
main = do
    allocaBytes 80 $ \buf -> do
        dds_ErrorMessage (-201) buf
        str <- peekCString buf
        putStrLn str
