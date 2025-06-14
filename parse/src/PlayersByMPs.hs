module PlayersByMPs (processEvents) where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import Text.Printf (printf)

import JsonTypes

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

processEvents :: [Event] -> IO ()
processEvents es = forM_ (zip [1::Int ..] plist) $ \(i, (name, mps)) ->
    printf "%3d. %8.2f %s\n" i mps name
  where
    plist = sortBy (comparing (Down . snd)) (M.toList $ getLatestMPs es)
