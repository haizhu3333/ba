module JsonTypes where

import Data.Aeson ((.:), FromJSON(..), Value(..), withObject, withText)
import Data.Aeson.Types (unexpected, Parser)
import Data.Text (Text)
import qualified Data.Text.Read as T
import Text.Printf (PrintfArg)

data PairDirection = NS | EW
    deriving (Eq, Ord, Show)

data Event = Event
    { startDate :: Text
    , createdAt :: Text
    , boardTop :: Float
    , sessions :: [Session]
    } deriving Show

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
    , matchPointsNS :: Float
    , matchPointsEW :: Float
    } deriving (Show)

newtype PairNumber = PairNumber Text
    deriving newtype (Eq, Ord, Show, FromJSON, PrintfArg)
newtype PlayerNumber = PlayerNumber Text
    deriving newtype (Eq, Ord, Show, FromJSON, PrintfArg)
newtype BoardNumber = BoardNumber Int
    deriving newtype (Eq, Ord, Show, FromJSON, PrintfArg)

instance FromJSON Event where
    parseJSON = withObject "Event" $ \v -> do
        startDate <- v .: "start_date"
        createdAt <- v .: "created_at"
        boardTop <- v .: "acbl_board_top" >>= parseTextFloat
        sessions <- v .: "sessions"
        pure Event{..}

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

parseTextFloat :: Text -> Parser Float
parseTextFloat t = case T.rational t of
    Right (x, "") -> pure x
    _ -> fail $ "Cannot parse number: " ++ show t

parseMPs :: Value -> Parser Float
parseMPs (String t) = parseTextFloat t
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
        matchPointsNS <- v .: "ns_match_points" >>= parseTextFloat
        matchPointsEW <- v .: "ew_match_points" >>= parseTextFloat
        pure BoardResult{..}
