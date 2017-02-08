module Main where

  import Data.Char
  -- [TODO] - Split this file in 2 if possible: 1 w/ Vector & 1 w/ List
  -- import Data.Vector
  import Data.List
  import System.IO

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

  main :: IO ()
  main = do
    boardIO <- readFile "data/board.txt"
    let board = readBoard boardIO
    -- putStr (boardToAscii board)
    gameTurn (board, White)
