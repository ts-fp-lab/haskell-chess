module Main where

  import Data.Char
  -- [TODO] - Split this file in 2 if possible: 1 w/ Vector & 1 w/ List
  -- import Data.Vector
  import Data.List
  import System.IO
  import System.Directory

  import Datatypes
  import Board
  import Rules



  chessCoords :: Coords -> String
  chessCoords (x, y) = [Data.Char.chr (x+65)] ++ (show (8 - y))

  chessMove :: Move -> String
  chessMove (origin, destination) = (chessCoords origin) ++ "->" ++ (chessCoords destination)

  -- indexesToCoords :: Int -> Int -> Coords
  -- indexesToCoords x y = (read $ (:[]) $ Data.Char.chr ((Data.Char.ord (head (show (minBound :: Letter))) + x)), 8 - y)

  stringToCoords :: Char -> Char -> Coords
  stringToCoords letter number = (x, y)
    where
      x = ((Data.Char.ord $ toUpper letter) - 65) `mod` 8
      y = (8 - read [number]) `mod` 8

  stringToMove :: String -> Move
  stringToMove (l1:n1:l2:n2:rest) = (stringToCoords l1 n1, stringToCoords l2 n2)
  -- writeBoard :: Board -> String
  -- writeBoard board = (intercalate "\n" $ toList $ Data.Vector.map (toList . Data.Vector.map (head . show)) board) ++ "\n"

  fromJust :: Maybe a -> a
  fromJust (Just a) = a

  --------
  -- IO --
  --------

  queryMove :: GameState -> IO Move
  queryMove gameState = do
    moveIO <- getLine
    let move = stringToMove moveIO
    if isMoveAllowed gameState move then
      return move
    else do
      putStrLn "Impossible Move. Retry!"
      putStrLn ("Possible Moves:" ++ concatMap chessCoords (possibleMoves gameState (fst move)))
      queryMove gameState

  gameTurn :: GameState -> IO ()
  gameTurn (board, color) = do
    putStr $ boardToAscii board
    putStr ((show color) ++ "'s turn. Move? (eg. d2d4)\n")
    move <- queryMove (board, color)
    let newBoard = makeMove board move
    let winner = gameOverWinner newBoard
    if winner == Nothing then
      gameTurn (newBoard, next color)
    else
      putStr ((show (fromJust winner)) ++ " won!")

  choosePlayer :: IO Color
  choosePlayer = do
    putStrLn "What player do you want to be? (W/B)"
    playerIO <- getLine
    if toUpper (head playerIO) == 'W' then
      return White
    else
      return Black


  newGame :: IO ()
  newGame = do
    putStrLn "Starting new game."
    boardIO <- readFile "data/board.txt"
    let board = readBoard boardIO
    startColor <- choosePlayer
    gameTurn (board, startColor)

  loadGame :: IO ()
  loadGame = do
    putStrLn "Loading game."
    availableGames <- getDirectoryContents "games"
    -- putStrLn availableGames
    putStrLn (intercalate " " availableGames)
    -- mapM_ (\game ->  game) availableGames
    -- map (\filePath -> putStrLn ( filePath )



  main :: IO ()
  main = do
    putStrLn "Welcome to Haskell Chess."
    putStrLn "What do you want to do?"
    putStrLn "  - 1. Start a new game"
    putStrLn "  - 2. Load a game"
    choiceIO <- getLine
    if choiceIO == "1" then
      newGame
    else
      loadGame


