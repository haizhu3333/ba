{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Ord
import Data.Text (Text)
import qualified Data.Text.Read as T
import Text.Printf

newtype Event = Event [Session]
    deriving Show

data Session = Session
    { sessionNum :: Int
    , handRecords :: [HandRecord]
    , sections :: [Section]
    } deriving (Show)

data HandRecord = HandRecord
    { boardNum :: Int
    , pointsN :: Int
    , pointsS :: Int
    , pointsE :: Int
    , pointsW :: Int
    } deriving (Show)

data Section = Section
    { sectionName :: Text
    , sectionPairs :: [PairSummary]
    } deriving (Show)

data PairSummary = PairSummary
    { pairNum :: Text
    , pairDir :: Text
    , playerNE :: Player
    , playerSW :: Player
    } deriving (Show)

data Player = Player
    { playerName :: Text
    , playerMPs :: Float
    } deriving (Show)

instance FromJSON Event where
    parseJSON = withObject "Event" $ \v -> Event <$> v .: "sessions"

instance FromJSON Session where
    parseJSON = withObject "Session" $ \v -> do
        sessionNum <- v .: "number"
        handRecords <- v .: "hand_records"
        sections <- v .: "sections"
        pure Session{..}

instance FromJSON HandRecord where
    parseJSON = withObject "HandRecord" $ \v -> do
        boardNum <- v .: "board"
        points <- v .: "points"
        pointsN <- points .: "N"
        pointsS <- points .: "S"
        pointsE <- points .: "E"
        pointsW <- points .: "W"
        pure HandRecord{..}

instance FromJSON Section where
    parseJSON = withObject "Section" $ \v -> do
        sectionName <- v .: "name"
        sectionPairs <- v .: "pair_summaries"
        pure Section{..}

instance FromJSON PairSummary where
    parseJSON = withObject "PairSummary" $ \v -> do
        pairNum <- v .: "pair_number"
        pairDir <- v .: "direction"
        players <- v .: "players"
        case players of
            [playerNE, playerSW] -> pure PairSummary{..}
            _ -> fail "Wrong number of players in pair_summary"

instance FromJSON Player where
    parseJSON = withObject "Player" $ \v -> do
        playerName <- v .: "name"
        mpValue <- v .: "mp_total"
        playerMPs <- getMPs mpValue
        pure Player{..}

getMPs :: Value -> Parser Float
getMPs (String t) = case T.rational t of
    Right (x, "") -> pure x
    _ -> fail $ "Cannot parse number: " ++ show t
getMPs Null = pure 0
getMPs v = unexpected v

allPlayers :: Event -> [Player]
allPlayers (Event sessions) = do
    session <- sessions
    section <- sections session
    pair <- sectionPairs section
    [playerNE pair, playerSW pair]

process :: Event -> IO ()
process event = do
    let players = allPlayers event
    let sorted = sortBy (comparing (Down . playerMPs)) players
    forM_ sorted $ \p -> printf "%8.2f  %s\n" (playerMPs p) (playerName p)

main :: IO ()
main = do
    decoded <- eitherDecodeFileStrict "../downloader/output/2025-05-27.json"
    case decoded of
        Left err -> putStrLn err
        Right json -> process json
