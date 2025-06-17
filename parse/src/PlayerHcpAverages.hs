module PlayerHcpAverages (processEvents) where

import Control.Monad (forM_, forM)
import Data.Aeson.Types (Result (..))
import Text.Printf (printf)

import JsonTypes
import Utils

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
