{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Monad
import Data.Bits
import qualified Data.Vector.Fixed as F
import qualified Data.Vector.Fixed.Storable as F
import Foreign
import Foreign.C
import Lens.Micro
import System.IO.Unsafe (unsafePerformIO)

import Structs

foreign import ccall unsafe "ErrorMessage"
    dds_ErrorMessage :: CInt -> Ptr CChar -> IO ()
foreign import ccall unsafe "CalcAllTables"
    dds_CalcAllTables :: Ptr DDTableDeals -> CInt -> Ptr CInt -> Ptr DDTablesRes -> Ptr ()
                      -> IO CInt

returnNoFault :: CInt
returnNoFault = 1

calcAllTables :: [DDTableDeal] -> IO [DDTableResults]
calcAllTables deals =
    with (DDTableDeals deals) $ \dealsPtr ->
    flip F.unsafeWith trumpFilter $ \trumpFilterPtr ->
    alloca $ \resPtr -> do
        fillBytes resPtr 0 (sizeOf (undefined :: DDTablesRes))
        retval <- dds_CalcAllTables dealsPtr modeNoPar trumpFilterPtr resPtr nullPtr
        if retval == returnNoFault
        then getDDTableResultsList <$> peekPartial resPtr (length deals)
        else fail (errorMessage retval)
  where
    modeNoPar :: CInt
    modeNoPar = -1

    trumpFilter :: F.Vec 5 CInt
    trumpFilter = F.fromList [0, 0, 0, 0, 0]

errorMessage :: CInt -> String
errorMessage x = unsafePerformIO $
    allocaBytes 80 $ \buf -> do
        dds_ErrorMessage x buf
        peekCString buf

suitAt :: Char -> Lens' (Suits a) a
suitAt 'S' f s = (\x -> s{spades   = x}) <$> f (spades s)
suitAt 'H' f s = (\x -> s{hearts   = x}) <$> f (hearts s)
suitAt 'D' f s = (\x -> s{diamonds = x}) <$> f (diamonds s)
suitAt 'C' f s = (\x -> s{clubs    = x}) <$> f (clubs s)
suitAt ch _ _ = error $ "Unknown suit " ++ show ch

rankFlag :: Char -> PackedSuit
rankFlag '2' = 0x0004
rankFlag '3' = 0x0008
rankFlag '4' = 0x0010
rankFlag '5' = 0x0020
rankFlag '6' = 0x0040
rankFlag '7' = 0x0080
rankFlag '8' = 0x0100
rankFlag '9' = 0x0200
rankFlag 'T' = 0x0400
rankFlag 'J' = 0x0800
rankFlag 'Q' = 0x1000
rankFlag 'K' = 0x2000
rankFlag 'A' = 0x4000
rankFlag ch = error $ "Unknown rank " ++ show ch

makeHand :: [(Char, Char)] -> Hand
makeHand = foldl' go (F.replicate 0)
  where
    go hand (s, r) = hand & suitAt s %~ (.|. rankFlag r)

strHand :: String -> String -> String -> String -> Hand
strHand s h d c = makeHand $ concat [('S' ,) <$> s, ('H' ,) <$> h, ('D' ,) <$> d, ('C', ) <$> c]

deal1, deal2, deal3 :: DDTableDeal
deal1 = DDTableDeal $ Hands
    (strHand "QJ6" "K652" "J85" "T98")
    (strHand "873" "J97" "AT764" "Q4")
    (strHand "K5" "T83" "KQ9" "A7652")
    (strHand "AT942" "AQ4" "32" "KJ3")
deal2 = DDTableDeal $ Hands
    (strHand "AK96" "KQ8" "A98" "K63")
    (strHand "QJT5432" "T" "6" "QJ82")
    (strHand "" "J97543" "K7532" "94")
    (strHand "87" "A62" "QJT4" "AT75")
deal3 = DDTableDeal $ Hands
    (strHand "73" "QJT" "AQ54" "T752")
    (strHand "QT6" "876" "KJ9" "AQ84")
    (strHand "5" "A95432" "7632" "K6")
    (strHand "AKJ9842" "K" "T8" "J93")

main :: IO ()
main = do
    res <- calcAllTables [deal1, deal2, deal3]
    print (length res)
    forM_ res print
