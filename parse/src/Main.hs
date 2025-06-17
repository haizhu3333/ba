module Main where

import Control.Monad (forM_)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Either (partitionEithers)
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath
import System.IO

import JsonTypes
import qualified HcpScoreCorr
import qualified PlayerHcpAverages
import qualified PlayersByMPs

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

withEventFiles :: FilePath -> ([Event] -> IO ()) -> IO ()
withEventFiles path f = do
    paths <- getFilePaths path
    (errors, es) <- partitionEithers <$> mapM labeledDecode paths
    forM_ errors (hPutStrLn stderr)
    f $ sortBy (comparing (.createdAt)) es

usage :: IO ()
usage = do
    hPutStrLn stderr $ unlines
        [ "Usage: "
        , "  ba-parse players <filepath>"
        , "  ba-parse hcp-avg <filepath> <player#>"
        , "  ba-parse hcp-corr <filepath> <player#>"
        ]
    exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["players", path] -> withEventFiles path PlayersByMPs.processEvents
        ["hcp-avg", path, pnum] -> withEventFiles path $ PlayerHcpAverages.processEvents (toPN pnum)
        ["hcp-corr", path, pnum] -> withEventFiles path $ HcpScoreCorr.processEvents (toPN pnum)
        _ -> usage
  where
    toPN = PlayerNumber . T.pack
