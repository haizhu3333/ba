module PlayerHcpAverages (processEvents) where

import Control.Monad (forM_, forM)
import Data.Aeson.Types (Result (..))
import Text.Printf (printf)

import JsonTypes
import Utils

data Played = Played
    { boards :: [Board]
    , pairNumber :: PairNumber
    , direction :: Direction
    } deriving (Show)

findPlayerInPair :: PlayerNumber -> Pair -> Result (PairNumber, Direction)
findPlayerInPair target pair
    | pair.player1.number == target = pure (pair.number, dir1)
    | pair.player2.number == target = pure (pair.number, dir2)
    | otherwise = fail $ printf "Player %s not found" target
  where
    (dir1, dir2) = case pair.direction of
        NS -> (N, S)
        EW -> (E, W)

hasPlayedBoard :: PairNumber -> Direction -> Board -> Bool
hasPlayedBoard pairNo direction board =
    or [br.contract /= "" && brPairNo br == pairNo | br <- board.results]
  where
    brPairNo :: BoardResult -> PairNumber
    brPairNo br = case direction of
        N -> br.pairNumberNS
        S -> br.pairNumberNS
        E -> br.pairNumberEW
        W -> br.pairNumberEW

findPlayerInSection :: PlayerNumber -> Section -> Result Played
findPlayerInSection target section =
    toPlayed <$> mapFirst (findPlayerInPair target) section.pairs
  where
    toPlayed (pairNumber, direction) =
        let boards = filter (hasPlayedBoard pairNumber direction) section.boards
        in  Played{..}

pointsByDir :: Direction -> HandRecord -> Int
pointsByDir N hr = hr.pointsN
pointsByDir S hr = hr.pointsS
pointsByDir E hr = hr.pointsE
pointsByDir W hr = hr.pointsW

pointsOfBoard :: Direction -> BoardNumber -> Session -> Result Int
pointsOfBoard dir bn session = mapFirst go session.handRecords
  where
    go hr | hr.boardNumber == bn = pure $ pointsByDir dir hr
          | otherwise = fail $ printf "Not found: board %d" bn

getPointsForPlayer :: PlayerNumber -> Session -> Result [(BoardNumber, Int)]
getPointsForPlayer target session = do
    p <- mapFirst (findPlayerInSection target) session.sections
    forM p.boards $ \b -> do
        points <- pointsOfBoard p.direction b.number session
        pure (b.number, points)

processEvents :: PlayerNumber -> [Event] -> IO ()
processEvents pn es =
    forM_ es $ \e -> do
        forM_ e.sessions $ \session ->
            case getPointsForPlayer pn session of
                Error err -> putStrLn err
                Success pts -> summarize e.startDate session pts
  where
    summarize eventDate session pts =
        let total = sum $ map snd pts
            avg = fromIntegral total / fromIntegral (length pts) :: Double
        in  printf "%s Session %d: Average %.2f HCP\n" eventDate session.number avg
