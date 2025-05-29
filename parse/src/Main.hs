{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative (Alternative)
import Control.Monad (forM, forM_)
import Data.Aeson (
    FromJSON(..), Value(..), Result(..),
    eitherDecodeFileStrict, (.:), withObject, withText)
import Data.Aeson.Types (unexpected, Parser)
import Data.List (sortBy)
import Data.Monoid (Alt(..))
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import System.Environment (getArgs)
import Text.Printf (PrintfArg, printf)

data Direction = N | S | E | W
    deriving (Eq, Ord, Show)

data PairDirection = NS | EW
    deriving (Eq, Ord, Show)

newtype Event = Event { sessions :: [Session] }
    deriving Show

data Session = Session
    { number :: Int
    , handRecords :: [HandRecord]
    , sections :: [Section]
    } deriving (Show)

data HandRecord = HandRecord
    { boardNumber :: BoardNumber
    , pointsN :: Int
    , pointsS :: Int
    , pointsE :: Int
    , pointsW :: Int
    } deriving (Show)

data Section = Section
    { name :: Text
    , pairs :: [Pair]
    , boards :: [Board]
    } deriving (Show)

data Pair = Pair
    { number :: PairNumber
    , direction :: PairDirection
    , player1 :: Player
    , player2 :: Player
    } deriving (Show)

data Player = Player
    { name :: Text
    , number :: PlayerNumber
    , totalMPs :: Float
    } deriving (Show)

data Board = Board
    { number :: BoardNumber
    , results :: [BoardResult]
    } deriving (Show)

data BoardResult = BoardResult
    { pairNumberNS :: PairNumber
    , pairNumberEW :: PairNumber
    , contract :: Text
    } deriving (Show)

newtype PairNumber = PairNumber Text
    deriving newtype (Eq, Ord, Show, FromJSON, PrintfArg)
newtype PlayerNumber = PlayerNumber Text
    deriving newtype (Eq, Ord, Show, FromJSON, PrintfArg)
newtype BoardNumber = BoardNumber Int
    deriving newtype (Eq, Ord, Show, FromJSON, PrintfArg)

instance FromJSON Event where
    parseJSON = withObject "Event" $ \v -> Event <$> v .: "sessions"

instance FromJSON Session where
    parseJSON = withObject "Session" $ \v -> do
        number <- v .: "number"
        handRecords <- v .: "hand_records"
        sections <- v .: "sections"
        pure Session{..}

instance FromJSON HandRecord where
    parseJSON = withObject "HandRecord" $ \v -> do
        boardNumber <- v .: "board"
        points <- v .: "points"
        pointsN <- points .: "N"
        pointsS <- points .: "S"
        pointsE <- points .: "E"
        pointsW <- points .: "W"
        pure HandRecord{..}

instance FromJSON Section where
    parseJSON = withObject "Section" $ \v -> do
        name <- v .: "name"
        pairs <- v .: "pair_summaries"
        boards <- v .: "boards"
        pure Section{..}

instance FromJSON Pair where
    parseJSON = withObject "PairSummary" $ \v -> do
        number <- v .: "pair_number"
        dirStr <- v .: "direction"
        direction <- parsePairDirection dirStr
        players <- v .: "players"
        case players of
            [player1, player2] -> pure Pair{..}
            _ -> fail "Wrong number of players in pair_summary"

parsePairDirection :: Value -> Parser PairDirection
parsePairDirection = withText "PairDirection" $ \v -> case v of
    "NS" -> pure NS
    "EW" -> pure EW
    _ -> fail $ "Invalid pair direction " ++ show v

instance FromJSON Player where
    parseJSON = withObject "Player" $ \v -> do
        name <- v .: "name"
        number <- v .: "id_number"
        mpObj <- v .: "mp_total"
        totalMPs <- parseMPs mpObj
        pure Player{..}

parseMPs :: Value -> Parser Float
parseMPs (String t) = case T.rational t of
    Right (x, "") -> pure x
    _ -> fail $ "Cannot parse number: " ++ show t
parseMPs Null = pure 0
parseMPs v = unexpected v

instance FromJSON Board where
    parseJSON = withObject "Board" $ \v -> do
        number <- v .: "board_number"
        results <- v .: "board_results"
        pure Board{..}

instance FromJSON BoardResult where
    parseJSON = withObject "BoardResult" $ \v -> do
        pairNumberNS <- v .: "ns_pair"
        pairNumberEW <- v .: "ew_pair"
        contract <- v .: "contract"
        pure BoardResult{..}

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
