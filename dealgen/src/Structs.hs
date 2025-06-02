{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
module Structs where

import Control.Monad
import Data.Proxy
import qualified Data.Vector.Fixed as F
import Foreign
import Foreign.C
import GHC.TypeNats

data Hands a = Hands {north :: a, east :: a, south :: a, west :: a}
    deriving (Show)
    deriving (Storable) via F.StorableViaFixed Hands a
type instance F.Dim Hands = 4
instance F.Vector Hands a where
    construct = F.Fun Hands
    inspect Hands{..} (F.Fun f) = f north east south west

data Suits a = Suits {spades :: a, hearts :: a, diamonds :: a, clubs :: a}
    deriving (Show)
    deriving (Storable) via F.StorableViaFixed Suits a
type instance F.Dim Suits = 4
instance F.Vector Suits a where
    construct = F.Fun Suits
    inspect Suits{..} (F.Fun f) = f spades hearts diamonds clubs

data Strains a = Strains {trump :: Suits a, notrump :: a}
    deriving (Show)
    deriving (Storable) via F.StorableViaFixed Strains a
type instance F.Dim Strains = 5
instance F.Vector Strains a where
    construct = F.Fun $ \spades hearts diamonds clubs nt -> Strains Suits{..} nt 
    inspect (Strains Suits{..} nt) (F.Fun f) = f spades hearts diamonds clubs nt

newtype LimitList (n :: Natural) a = LimitList [a] deriving (Show)
instance (KnownNat n, Storable a) => Storable (LimitList n a) where
    sizeOf _ = sizeOf (0 :: CInt) + fromIntegral (natVal $ Proxy @n) * sizeOf (undefined :: a)
    alignment _ = alignment (0 :: CInt) `max` alignment (undefined :: a)
    peek ptr = do
        count <- (fromIntegral :: CInt -> Int) <$> peek (castPtr ptr)
        peekPartial ptr count
    poke ptr (LimitList xs) = do
        let count = length xs
            limit = fromIntegral (natVal $ Proxy @n)
        when (count > limit) $
            fail $ "Limit exceeded on write: " ++ show count ++ "/" ++ show limit
        poke (castPtr ptr) (fromIntegral count :: CInt)
        pokeArray (castPtr ptr `plusPtr` sizeOf (0 :: CInt)) xs

class Storable a => PartialStorable a where
    peekPartial :: Ptr a -> Int -> IO a
instance (KnownNat n, Storable a) => PartialStorable (LimitList n a) where
    peekPartial ptr count = do
        let limit = fromIntegral (natVal $ Proxy @n)
        when (count > limit) $
            fail $ "Limit exceeded on read: " ++ show count ++ "/" ++ show limit
        LimitList <$> peekArray count (castPtr ptr `plusPtr` sizeOf (0 :: CInt))

type PackedSuit = CInt
type Hand = Suits PackedSuit

newtype DDTableDeals = DDTableDeals [DDTableDeal]
    deriving (Show)
    deriving (Storable, PartialStorable) via LimitList 200 DDTableDeal
newtype DDTableDeal = DDTableDeal (Hands Hand) deriving (Show, Storable)

newtype DDTablesRes = DDTablesRes { getDDTableResultsList :: [DDTableResults] }
    deriving (Show)
    deriving (Storable, PartialStorable) via LimitList 200 DDTableResults
newtype DDTableResults = DDTableResults (Strains (Hands CInt)) deriving (Show, Storable)
