module Main (main) where

import Control.Monad (replicateM)
import Data.Bits (Bits(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import qualified Data.Vector.Fixed as F
import Foreign (Ptr, sizeOf, alloca, allocaBytes, fillBytes, with, nullPtr)
import Foreign.C (CChar, CInt(..), peekCString)
import Lens.Micro ((&), (%~), (^.), Lens')
import Lens.Micro.Extras (view)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (stderr, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.Stateful (StatefulGen, initStdGen, newIOGenM, uniformShuffleListM)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Structs

foreign import ccall unsafe "ErrorMessage"
    dds_ErrorMessage :: CInt -> Ptr CChar -> IO ()
foreign import ccall unsafe "CalcAllTables"
    dds_CalcAllTables :: Ptr DDTableDeals   -- deals
                      -> CInt               -- mode
                      -> Ptr (Strains CInt) -- trumpFilter[5]
                      -> Ptr DDTablesRes    -- resp
                      -> Ptr ()             -- presp (unused)
                      -> IO CInt

returnNoFault :: CInt
returnNoFault = 1

calcAllTables :: [DDTableDeal] -> IO [DDTableResults]
calcAllTables deals =
    with (DDTableDeals deals) $ \dealsPtr ->
    with trumpFilter $ \tfPtr ->
    alloca $ \resPtr -> do
        fillBytes resPtr 0 (sizeOf (undefined :: DDTablesRes))
        retval <- dds_CalcAllTables dealsPtr modeNoPar tfPtr resPtr nullPtr
        if retval == returnNoFault
        then view ddResults <$> peekPartial resPtr (length deals)
        else fail (errorMessage retval)
  where
    modeNoPar :: CInt
    modeNoPar = -1

    trumpFilter :: Strains CInt
    trumpFilter = F.mk5 1 1 1 1 0

errorMessage :: CInt -> String
errorMessage x = unsafePerformIO $
    allocaBytes 80 $ \buf -> do
        dds_ErrorMessage x buf
        peekCString buf

suitIndex :: Char -> Int
suitIndex 'S' = 0
suitIndex 'H' = 1
suitIndex 'D' = 2
suitIndex 'C' = 3
suitIndex s = error $ "Unknown suit " ++ show s

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

data Card = Card Char Char deriving (Eq, Ord)

instance Show Card where
    show (Card s r) = [s, r]

makeHand :: [Card] -> Hand
makeHand = foldl' go (F.replicate 0)
  where
    go hand (Card s r) = hand & F.element (suitIndex s) %~ (.|. rankFlag r)

randomDeal :: StatefulGen g m => g -> m (Hands [Card], DDTableDeal)
randomDeal rng = do
    deck0 <- uniformShuffleListM deck rng
    let (n, deck1) = splitAt 13 deck0
        (e, deck2) = splitAt 13 deck1
        (s, w) = splitAt 13 deck2
        cards = Hands n e s w
        deal = DDTableDeal $ Hands (makeHand n) (makeHand e) (makeHand s) (makeHand w)
    pure (cards, deal)
  where
    deck = Card <$> "SHDC" <*> "23456789TJQKA"

randomDealBatch :: StatefulGen g IO => Int -> g -> IO [(Hands [Card], Hands Int)]
randomDealBatch n rng = do
    (cards, deals) <- unzip <$> replicateM n (randomDeal rng)
    printf "Solving %d deals\n" n
    resList <- calcAllTables deals
    let ntRes = [ fromIntegral <$> r ^. notrump | DDTableResults r <- resList ]
    pure $ zip cards ntRes

sortBySuit :: [Card] -> Suits String
sortBySuit cards = Suits (get 'S') (get 'H') (get 'D') (get 'C')
  where
    get suit = sortBy (comparing (Down . rankFlag)) [ r | Card s r <- cards, s == suit ]

toRecords :: Hands [Card] -> Hands Int -> [CSV.Record]
toRecords cards tricks = [toR north, toR east, toR south, toR west]
  where
    toR :: (forall a. Lens' (Hands a) a) -> CSV.Record
    toR dir =
        let suits = sortBySuit (cards ^. dir)
        in  CSV.toRecord
                (suits ^. spades, suits ^. hearts, suits ^. diamonds, suits ^. clubs, tricks ^. dir)

generateCsv :: Int -> FilePath -> IO ()
generateCsv nBatch outputPath = do
    rng <- initStdGen >>= newIOGenM
    batches <- replicateM nBatch (randomDealBatch 200 rng)
    let records = concatMap (uncurry toRecords) (concat batches)
    BL.writeFile outputPath $ CSV.encode records

main :: IO ()
main = do
    args <- getArgs
    case args of
        [nBatchStr, outputPath] | Just nBatch <- readMaybe nBatchStr ->
            generateCsv nBatch outputPath
        _ -> do
            hPutStrLn stderr "Usage: ba-dealgen <num batches> <output path>"
            exitFailure
