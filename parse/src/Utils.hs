module Utils (
    Direction(..), Played(..), mapFirst, pointsByDir, findPlayerInSection
) where

import Control.Applicative (Alternative)
import Data.Aeson (Result)
import Data.Monoid (Alt(..))
import Text.Printf (printf)

import JsonTypes

data Direction = N | S | E | W
    deriving (Eq, Ord, Show)

data Played = Played
    { boards :: [Board]
    , pairNumber :: PairNumber
    , direction :: Direction
    } deriving (Show)

mapFirst :: (Alternative m, Foldable f) => (a -> m b) -> f a -> m b
mapFirst f xs = getAlt $ foldMap (Alt . f) xs

pointsByDir :: Direction -> HandRecord -> Int
pointsByDir N hr = hr.pointsN
pointsByDir S hr = hr.pointsS
pointsByDir E hr = hr.pointsE
pointsByDir W hr = hr.pointsW

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
