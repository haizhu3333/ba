module Main where

import Control.Exception (mask, onException)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, allocate, runResourceT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Database.SQLite3 as DB
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import JsonTypes
import Paths_ba_parse (getDataFileName)
import Data.Coerce (coerce)
import System.FilePath (takeDirectory)

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
    f $ sortBy (comparing (.createdAt)) es

insertEventSql :: Text
insertEventSql =
    "INSERT INTO Events (id, startDate, boardTop) VALUES (?, ?, ?)"

insertHandRecordSql :: Text
insertHandRecordSql = T.concat
    [ "INSERT INTO HandRecords (eventId, sessionNo, boardNo, pointsN, pointsS, pointsE, pointsW) "
    , "VALUES (?, ?, ?, ?, ?, ?, ?)"
    ]

insertPlayerSql :: Text
insertPlayerSql = T.concat
    [ "INSERT INTO Players (eventId, sessionNo, sectionName, "
    , "                     pairNo, direction, name, number, totalMPs) "
    , "VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
    ]

insertResultSql :: Text
insertResultSql = T.concat
    [ "INSERT INTO Results (eventId, sessionNo, sectionName, boardNo, tableNo, "
    , "                     pairNS, pairEW, scoreNS, scoreEW) "
    , "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ]

createDatabase :: FilePath -> ResourceT IO DB.Database
createDatabase dbPath = do
    fileExists <- liftIO $ doesFileExist dbPath
    when fileExists $ liftIO $ removeFile dbPath
    liftIO $ createDirectoryIfMissing False (takeDirectory dbPath)
    (_, db) <- allocate (DB.open (T.pack dbPath)) DB.close
    schemaPath <- liftIO $ getDataFileName "schema.sql"
    schema <- liftIO $ T.readFile schemaPath
    liftIO $ DB.exec db schema
    pure db

data Inserter = Inserter
    { db :: DB.Database
    , insertEventStmt :: DB.Statement
    , insertHandRecordStmt :: DB.Statement
    , insertPlayerStmt :: DB.Statement
    , insertResultStmt :: DB.Statement
    }

makeInserter :: DB.Database -> ResourceT IO Inserter
makeInserter db = do
    let toStmt sql = snd <$> allocate (DB.prepare db sql) DB.finalize
    insertEventStmt <- toStmt insertEventSql
    insertHandRecordStmt <- toStmt insertHandRecordSql
    insertPlayerStmt <- toStmt insertPlayerSql
    insertResultStmt <- toStmt insertResultSql
    pure Inserter{..}

runStatement :: DB.Statement -> IO ()
runStatement stmt = do
    stepResult <- DB.step stmt
    case stepResult of
        DB.Row -> runStatement stmt
        DB.Done -> DB.reset stmt

withTransaction :: DB.Database -> IO a -> IO a
withTransaction db io = mask $ \restore -> do
    DB.exec db "BEGIN TRANSACTION"
    (restore io <* DB.exec db "COMMIT TRANSACTION")
        `onException`
        DB.exec db "ROLLBACK TRANSACTION"

processEvents :: FilePath -> [Event] -> IO ()
processEvents dbPath es = runResourceT $ do
    db <- createDatabase dbPath
    ins <- makeInserter db
    liftIO $ mapM_ (processEvent ins) es

processEvent :: Inserter -> Event -> IO ()
processEvent ins e = withTransaction ins.db $ do
    insertEvent ins e
    mapM_ (processSession ins e.objectId) e.sessions

insertEvent :: Inserter -> Event -> IO ()
insertEvent Inserter{insertEventStmt = stmt} e = do
    DB.bindInt stmt (DB.ParamIndex 1) e.objectId
    DB.bindText stmt (DB.ParamIndex 2) e.startDate
    DB.bindDouble stmt (DB.ParamIndex 3) e.boardTop
    runStatement stmt

processSession :: Inserter -> Int -> Session -> IO ()
processSession ins eventId session = do
    mapM_ (insertHandRecord ins eventId session.number) session.handRecords
    mapM_ (processSection ins eventId session.number) session.sections

insertHandRecord :: Inserter -> Int -> Int -> HandRecord -> IO ()
insertHandRecord Inserter{insertHandRecordStmt = stmt} eventId sessionNumber hr = do
    DB.bindInt stmt (DB.ParamIndex 1) eventId
    DB.bindInt stmt (DB.ParamIndex 2) sessionNumber
    DB.bindInt stmt (DB.ParamIndex 3) (coerce hr.boardNumber)
    DB.bindInt stmt (DB.ParamIndex 4) hr.pointsN
    DB.bindInt stmt (DB.ParamIndex 5) hr.pointsS
    DB.bindInt stmt (DB.ParamIndex 6) hr.pointsE
    DB.bindInt stmt (DB.ParamIndex 7) hr.pointsW
    runStatement stmt

processSection :: Inserter -> Int -> Int -> Section -> IO ()
processSection ins eventId sessionNumber section = do
    mapM_ (insertPair ins eventId sessionNumber section.name) section.pairs
    mapM_ (processBoard ins eventId sessionNumber section.name) section.boards

insertPair :: Inserter -> Int -> Int -> Text -> Pair -> IO ()
insertPair Inserter{insertPlayerStmt = stmt} eventId sessionNumber sectionName pair =
    case pair.direction of
        NS -> do insertPlayer "N" pair.player1
                 insertPlayer "S" pair.player2
        EW -> do insertPlayer "E" pair.player1
                 insertPlayer "W" pair.player2
  where
    insertPlayer direction player = do
        DB.bindInt stmt (DB.ParamIndex 1) eventId
        DB.bindInt stmt (DB.ParamIndex 2) sessionNumber
        DB.bindText stmt (DB.ParamIndex 3) sectionName
        DB.bindText stmt (DB.ParamIndex 4) (coerce pair.number)
        DB.bindText stmt (DB.ParamIndex 5) direction
        DB.bindText stmt (DB.ParamIndex 6) player.name
        DB.bindText stmt (DB.ParamIndex 7) (coerce player.number)
        case player.totalMPs of
            Nothing -> DB.bindNull stmt (DB.ParamIndex 8)
            Just mps -> DB.bindDouble stmt (DB.ParamIndex 8) mps
        runStatement stmt

processBoard :: Inserter -> Int -> Int -> Text -> Board -> IO ()
processBoard ins eventId sessionNumber sectionName board =
    mapM_ (insertResult ins eventId sessionNumber sectionName board.number) board.results

insertResult :: Inserter -> Int -> Int -> Text -> BoardNumber -> BoardResult -> IO ()
insertResult Inserter{insertResultStmt = stmt}
             eventId sessionNumber sectionName (BoardNumber boardNumber) br =
    unless (br.contract == "") $ do
        DB.bindInt stmt (DB.ParamIndex 1) eventId
        DB.bindInt stmt (DB.ParamIndex 2) sessionNumber
        DB.bindText stmt (DB.ParamIndex 3) sectionName
        DB.bindInt stmt (DB.ParamIndex 4) boardNumber
        DB.bindInt stmt (DB.ParamIndex 5) br.tableNumber
        DB.bindText stmt (DB.ParamIndex 6) (coerce br.pairNumberNS)
        DB.bindText stmt (DB.ParamIndex 7) (coerce br.pairNumberEW)
        DB.bindDouble stmt (DB.ParamIndex 8) br.matchPointsNS
        DB.bindDouble stmt (DB.ParamIndex 9) br.matchPointsEW
        runStatement stmt

usage :: IO ()
usage = do
    hPutStrLn stderr "Usage: ba-parse <db path> <data file path> ..."
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        dbPath : dataPaths -> withEventFiles dataPaths (processEvents dbPath)
        [] -> usage
