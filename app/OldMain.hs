module OldMain where

import Control.Monad.State
import System.Random

type PlayerAction = Int

type GameOver = Bool

data Player = Player1 | Player2
  deriving (Show)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

type TileState = Maybe Player

data GameState = GameState
  { board :: [TileState],
    currentMove :: Player,
    generator :: StdGen
  }
  deriving (Show)

newGameState :: GameState
newGameState = GameState [Nothing | _x <- [0 .. 9]] Player1 (mkStdGen 1)

isTileFree :: TileState -> Bool
isTileFree t = case t of
  Just x -> False
  Nothing -> True

chooseRandomMove :: StateT GameState IO PlayerAction
chooseRandomMove = do
  gameState <- get
  let gen = generator gameState
  let openSpots = filter (\(i, t) -> isTileFree t) $ zip [0 ..] (board gameState)
  let (i, gen') = randomR (0, length openSpots - 1) gen
  put $ gameState {generator = gen'}
  let (action, _) = openSpots !! i
  return action

applyMove :: PlayerAction -> StateT GameState IO ()
applyMove m = do
  gameState <- get
  let p = currentMove gameState
  let (b1, _ : b2) = splitAt m $ board gameState
  let newBoard = b1 ++ [Just p] ++ b2
  put $ gameState {currentMove = nextPlayer p, board = newBoard}
  return ()

hasPlayerWon :: Player -> StateT GameState IO Bool
hasPlayerWon p = do
  gameState <- get
  let b = board gameState
  return
    ( case p of
        Player1 -> case b of
          (Just Player1 : Just Player1 : Just Player1 : _) -> True
          _ -> False
        Player2 -> case b of
          (Just Player2 : Just Player2 : Just Player2 : _) -> True
          _ -> False
    )

isGameDone :: StateT GameState IO Bool
isGameDone = do
  gameState <- get
  let openSpots = filter isTileFree $ board gameState
  return $ length openSpots == 0

showBoard :: StateT GameState IO ()
showBoard = do
  gameState <- get
  let b = board gameState
  lift (print $ show (b !! 0) ++ show (b !! 1) ++ show (b !! 2))
  lift (print $ show (b !! 3) ++ show (b !! 4) ++ show (b !! 5))
  lift (print $ show (b !! 6) ++ show (b !! 7) ++ show (b !! 8))
  return ()

playGame :: StateT GameState IO (Maybe Player)
playGame = do
  gameState <- get
  move <- chooseRandomMove
  applyMove move
  gameDone <- isGameDone
  showBoard
  if gameDone
    then return Nothing
    else do
      let player = nextPlayer $ currentMove gameState
      playerWon <- hasPlayerWon player
      if playerWon
        then return (Just player)
        else playGame

main :: IO ()
main = do
  let winner = evalStateT playGame newGameState
  winner <- winner
  print winner
