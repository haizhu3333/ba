module HcpScoreCorr (processEvents) where

import Control.Monad (forM_)
import Data.Aeson (Result(..))
import Data.Map (Map)
import qualified Data.Map as M
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

import JsonTypes
import Utils

processEvents :: PlayerNumber -> [Event] -> IO ()
processEvents pnum es = case concat <$> mapM (recordsFromEvent pnum) es of
    Error err -> hPutStrLn stderr err
    Success records -> do
        putStrLn "hcp,mpRatio"
        forM_ records $ \r -> printf "%d,%f\n" r.hcp r.mpRatio

data Record = Record { hcp :: Int, mpRatio :: Float }
    deriving (Show)

recordsFromEvent :: PlayerNumber -> Event -> Result [Record]
recordsFromEvent pnum e = concat <$> mapM (recordsFromSession pnum e.boardTop) e.sessions

recordsFromSession :: PlayerNumber -> Float -> Session -> Result [Record]
recordsFromSession pnum top s = do
    played <- mapFirst (findPlayerInSection pnum) s.sections
    let hrMap = M.fromList [ (hr.boardNumber, pointsByDir played.direction hr)
                           | hr <- s.handRecords]
    mapM (toRecord hrMap played.pairNumber played.direction top) played.boards

toRecord :: Map BoardNumber Int -> PairNumber -> Direction -> Float -> Board -> Result Record
toRecord hrMap pnum dir top b = do
    hcp <- case M.lookup b.number hrMap of
        Nothing -> fail $ "Board not found in hand records: " ++ show b.number
        Just x -> pure x
    mps <- getMatchpoints pnum dir b
    let mpRatio = mps / top
    pure Record{..}

getMatchpoints :: PairNumber -> Direction -> Board -> Result Float
getMatchpoints pnum dir b = mapFirst match b.results
  where
    pairDir = case dir of
        N -> NS
        S -> NS
        E -> EW
        W -> EW
    match br = case pairDir of
        NS | br.pairNumberNS == pnum -> pure br.matchPointsNS
        EW | br.pairNumberEW == pnum -> pure br.matchPointsEW
        _ -> fail "No pair/direction match"
