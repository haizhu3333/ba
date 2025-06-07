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
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid (Alt(..))
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import System.IO
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

getLatestMPs :: [Event] -> Map Text Float
getLatestMPs es = M.fromListWith max $ do
    e <- es
    p <- allPlayers e
    pure (p.name, p.totalMPs)

printMPs :: [Event] -> IO ()
printMPs es = forM_ (zip [1::Int ..] plist) $ \(i, (name, mps)) ->
    printf "%3d. %8.2f %s\n" i mps name
  where
    plist = sortBy (comparing (Down . snd)) (M.toList $ getLatestMPs es)

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

summarizeSessions :: PlayerNumber -> [Event] -> IO ()
summarizeSessions pn es =
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

getFilePaths :: FilePath -> IO [FilePath]
getFilePaths path = do
    isFile <- doesFileExist path
    if isFile
    then do
        if takeExtension path == ".json"
        then pure [path]
        else pure []
    else do
        isDir <- doesDirectoryExist path
        if isDir
        then do
            children <- listDirectory path
            concat <$> mapM (getFilePaths . (path </>)) children
        else fail $ "Invalid path " ++ path

labeledDecode :: FilePath -> IO (Either String Event)
labeledDecode path = do
    decoded <- eitherDecodeFileStrict path
    case decoded of
        Left err -> pure $ Left (path ++ ": " ++ err)
        Right e -> pure $ Right e

withEventFiles :: [FilePath] -> ([Event] -> IO ()) -> IO ()
withEventFiles paths f = do
    (errors, es) <- partitionEithers <$> mapM labeledDecode paths
    forM_ errors (hPutStrLn stderr)
    f $ sortBy (comparing (.startDate)) es

usage :: IO ()
usage = do
    hPutStrLn stderr "Usage: ba-results-parse filepath [player#]"
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> usage
        jsonPath : args1 -> do
            filePaths <- getFilePaths jsonPath
            case args1 of
                [] -> withEventFiles filePaths printMPs
                [playerNo] -> withEventFiles filePaths $
                    summarizeSessions (PlayerNumber $ T.pack playerNo)
                _ -> usage
