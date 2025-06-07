{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (Alternative)
import Control.Monad (forM, forM_)
import Data.Aeson (Result(..), eitherDecodeFileStrict)
import Data.List (sortBy)
import Data.Monoid (Alt(..))
import Data.Ord (comparing, Down(..))
import qualified Data.Text as T
import System.Environment (getArgs)
import Text.Printf (printf)

import JsonTypes

data Direction = N | S | E | W
    deriving (Eq, Ord, Show)

mapFirst :: (Alternative m, Foldable f) => (a -> m b) -> f a -> m b
mapFirst f xs = getAlt $ foldMap (Alt . f) xs

allPlayers :: Event -> [Player]
allPlayers e = do
    session <- e.sessions
    section <- session.sections
    pair <- section.pairs
    [pair.player1, pair.player2]

printMPs :: Event -> IO ()
printMPs e = forM_ (sortPlayers $ allPlayers e) $ \p ->
    printf "%8.2f %s\n" p.totalMPs p.name
  where
    sortPlayers = sortBy $ comparing $ Down . (.totalMPs)

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
    fmap toPlayed $ mapFirst (findPlayerInPair target) section.pairs
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

summarizeSessions :: PlayerNumber -> Event -> IO ()
summarizeSessions pn e =
    forM_ e.sessions $ \session ->
        case getPointsForPlayer pn session of
            Error err -> putStrLn err
            Success pts -> summarize session pts
  where
    summarize session pts =
        let total = sum $ map snd pts
            avg = fromIntegral total / fromIntegral (length pts) :: Double
        in  printf "Session %d: Average %.2f HCP\n" session.number avg

withEventFile :: FilePath -> (Event -> IO ()) -> IO ()
withEventFile path f = do
    decoded <- eitherDecodeFileStrict path
    case decoded of
        Left err -> putStrLn err
        Right e -> f e

main :: IO ()
main = do
    args <- getArgs
    case args of
        [jsonFile] -> withEventFile jsonFile printMPs
        [jsonFile, playerNo] ->
            withEventFile jsonFile $
                summarizeSessions $ PlayerNumber $ T.pack playerNo
        _ -> putStrLn "Usage: ba-results-parse filepath [player#]"
