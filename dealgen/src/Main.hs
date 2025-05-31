{-# LANGUAGE StrictData #-}
module Main where

import Control.Monad
import Data.Bits
import Data.Proxy
import qualified Data.Vector.Fixed as F
import Foreign
import Foreign.C
import GHC.TypeNats
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall unsafe "ErrorMessage"
    dds_ErrorMessage :: CInt -> Ptr CChar -> IO ()
foreign import ccall unsafe "CalcAllTables"
    dds_CalcAllTables :: Ptr DDTableDeals -> CInt -> Ptr CInt -> Ptr DDTablesRes -> Ptr a -> IO CInt

newtype LimitList (n :: Natural) a = LimitList [a] deriving (Show)
instance (KnownNat n, Storable a) => Storable (LimitList n a) where
    sizeOf _ = sizeOf (0 :: CInt) + fromIntegral (natVal $ Proxy @n) * sizeOf (undefined :: a)
    alignment _ = alignment (0 :: CInt) `max` alignment (undefined :: a)
    peek ptr = do
        count <- (fromIntegral :: CInt -> Int) <$> peek (castPtr ptr)
        LimitList <$> peekLimitList count ptr
    poke ptr (LimitList xs) = do
        let count = length xs
            limit = fromIntegral (natVal $ Proxy @n)
        when (count > limit) $
            fail $ "Limit exceeded: " ++ show count ++ "/" ++ show limit
        poke (castPtr ptr) (fromIntegral count :: CInt)
        pokeArray (castPtr ptr `plusPtr` sizeOf (0 :: CInt)) xs

peekLimitList :: Storable a => Int -> Ptr (LimitList n a) -> IO [a]
peekLimitList count ptr =
    peekArray count (castPtr ptr `plusPtr` sizeOf (0 :: CInt))

type DDTableDeals = LimitList 200 DDTableDeal
newtype DDTableDeal = DDTableDeal (F.VecList 4 DDHand) deriving (Show, Storable)
newtype DDHand = DDHand (F.VecList 4 CInt) deriving (Show, Storable)

type DDTablesRes = LimitList 200 DDTableResults
newtype DDTableResults = DDTableResults (F.VecList 5 DDStrainResults) deriving (Show, Storable)
newtype DDStrainResults = DDStrainResults (F.VecList 4 CInt) deriving (Show, Storable)

returnNoFault :: CInt
returnNoFault = 1

data Card = Card !Suit !Rank deriving (Eq, Ord, Show)
data Suit = Spade | Heart | Diamond | Club deriving (Eq, Ord, Show, Enum)
data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | RJ | RQ | RK | RA
    deriving (Eq, Ord, Show, Enum)

makeHand :: [Card] -> DDHand
makeHand = go 0 0 0 0
  where
    go s h d c [] = DDHand $ F.mk4 s h d c
    go s h d c (Card suit rank : cards) =
        let rv = 0x4 `shiftL` fromEnum rank
        in  case suit of
                Spade   -> go (s .|. rv) h d c cards
                Heart   -> go s (h .|. rv) d c cards
                Diamond -> go s h (d .|. rv) c cards
                Club    -> go s h d (c .|. rv) cards

calcAllTables :: [DDTableDeal] -> IO [DDTableResults]
calcAllTables deals =
    with (LimitList deals) $ \dealsPtr ->
    withArray trumpFilter $ \trumpFilterPtr ->
    alloca $ \resPtr -> do
        fillBytes resPtr 0 (sizeOf (undefined :: DDTablesRes))
        retval <- dds_CalcAllTables dealsPtr modeNoPar trumpFilterPtr resPtr nullPtr
        if retval == returnNoFault
        then peekLimitList (length deals) resPtr
        else fail (errorMessage retval)
  where
    modeNoPar :: CInt
    modeNoPar = -1

    trumpFilter :: [CInt]
    trumpFilter = [1, 1, 1, 1, 0]

errorMessage :: CInt -> String
errorMessage x = unsafePerformIO $
    allocaBytes 80 $ \buf -> do
        dds_ErrorMessage x buf
        peekCString buf

strHand :: String -> String -> String -> String -> DDHand
strHand s h d c = makeHand $ concat
    [ Card Spade   <$> map toR s
    , Card Heart   <$> map toR h
    , Card Diamond <$> map toR d
    , Card Club    <$> map toR c
    ]
  where
    toR '2' = R2
    toR '3' = R3
    toR '4' = R4
    toR '5' = R5
    toR '6' = R6
    toR '7' = R7
    toR '8' = R8
    toR '9' = R9
    toR 'T' = R10
    toR 'J' = RJ
    toR 'Q' = RQ
    toR 'K' = RK
    toR 'A' = RA
    toR _ = error "bad rank"

deal1, deal2, deal3 :: DDTableDeal
deal1 = DDTableDeal $ F.mk4
    (strHand "QJ6" "K652" "J85" "T98")
    (strHand "873" "J97" "AT764" "Q4")
    (strHand "K5" "T83" "KQ9" "A7652")
    (strHand "AT942" "AQ4" "32" "KJ3")
deal2 = DDTableDeal $ F.mk4
    (strHand "AK96" "KQ8" "A98" "K63")
    (strHand "QJT5432" "T" "6" "QJ82")
    (strHand "" "J97543" "K7532" "94")
    (strHand "87" "A62" "QJT4" "AT75")
deal3 = DDTableDeal $ F.mk4
    (strHand "73" "QJT" "AQ54" "T752")
    (strHand "QT6" "876" "KJ9" "AQ84")
    (strHand "5" "A95432" "7632" "K6")
    (strHand "AKJ9842" "K" "T8" "J93")

main :: IO ()
main = do
    res <- calcAllTables [deal1, deal2, deal3]
    print (length res)
    forM_ res print
