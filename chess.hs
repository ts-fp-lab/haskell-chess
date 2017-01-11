module Main where

import Data.Char
-- [TODO] - Split this file in 2 if possible: 1 w/ Vector & 1 w/ List
import Data.Vector
import Data.List
import System.IO

data PieceType = Pawn | King | Queen | Rook | Bishop | Knight deriving (Show)
data Letter = A | B | C | D | E | F | G | H deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- [TODO] - Create Number class constrained between 1 & 8
-- data Number = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Color = Black | White deriving (Eq, Show)
type PieceInfo = (PieceType, Color)
-- [TODO] -- Use two bounded Ints & refactor
type Coords = (Letter, Int)
type Move = (Coords, Coords)

data Square = Empty | Piece PieceInfo
type Board = Vector (Vector Square)

instance Show Square where
  show (Empty) = "."
  show (Piece (Knight, White)) = "N"
  show (Piece (Knight, Black)) = "n"
  show (Piece (pieceType, White)) = [Data.List.head (show pieceType)]
  show (Piece (pieceType, Black)) = [toLower $ Data.List.head (show pieceType)]

instance Read Square where
  readsPrec d ('k': therest) = [(Piece (King, Black), therest)]
  readsPrec d ('q': therest) = [(Piece (Queen, Black), therest)]
  readsPrec d ('r': therest) = [(Piece (Rook, Black), therest)]
  readsPrec d ('b': therest) = [(Piece (Bishop, Black), therest)]
  readsPrec d ('n': therest) = [(Piece (Knight, Black), therest)]
  readsPrec d ('p': therest) = [(Piece (Pawn, Black), therest)]
  readsPrec d ('K': therest) = [(Piece (King, White), therest)]
  readsPrec d ('Q': therest) = [(Piece (Queen, White), therest)]
  readsPrec d ('R': therest) = [(Piece (Rook, White), therest)]
  readsPrec d ('B': therest) = [(Piece (Bishop, White), therest)]
  readsPrec d ('N': therest) = [(Piece (Knight, White), therest)]
  readsPrec d ('P': therest) = [(Piece (Pawn, White), therest)]
  readsPrec d ('.': therest) = [(Empty, therest)]
  readsPrec d _ = []

squareToAscii :: Square -> String
squareToAscii Empty = "."
squareToAscii (Piece (King, Black)) = "♚"
squareToAscii (Piece (Queen, Black)) = "♛"
squareToAscii (Piece (Rook, Black)) = "♜"
squareToAscii (Piece (Bishop, Black)) = "♝"
squareToAscii (Piece (Knight, Black)) = "♞"
squareToAscii (Piece (Pawn, Black)) = "♟"
squareToAscii (Piece (King, White)) = "♔"
squareToAscii (Piece (Queen, White)) = "♕"
squareToAscii (Piece (Rook, White)) = "♖"
squareToAscii (Piece (Bishop, White)) = "♗"
squareToAscii (Piece (Knight, White)) = "♘"
squareToAscii (Piece (Pawn, White)) = "♙"

boardLineToAscii :: Int -> Vector Square -> String
boardLineToAscii nb line = (show nb) Data.List.++ " ║" Data.List.++ (unwords (Data.List.map squareToAscii (toList line))) Data.List.++ "║ "  Data.List.++ (show nb)

boardToAscii :: Board -> String
boardToAscii board = unlines ([
  "   A B C D E F G H   ",
  "  ╔═══════════════╗  "] Data.List.++
  (toList (Data.Vector.imap (\lineNb line -> boardLineToAscii (8 - lineNb) line) board)) Data.List.++
  ["  ╚═══════════════╝  ",
  "   A B C D E F G H   "
  ])

next :: Color -> Color
next White = Black
next Black = White

indexesToCoords :: Int -> Int -> Coords
indexesToCoords x y = (read $ (:[]) $ Data.Char.chr ((Data.Char.ord (Data.List.head (show (minBound :: Letter))) + x)), 8 - y)

stringToCoords :: Char -> Char -> Coords
stringToCoords letter number = ((read [toUpper letter]), read [number])

stringToMove :: String -> Move
stringToMove (l1:n1:l2:n2:rest) = (stringToCoords l1 n1, stringToCoords l2 n2)

letterToInt :: Letter -> Int
letterToInt l = (Data.Char.ord (Data.List.head $ show l)) - (Data.Char.ord (Data.List.head $ show (minBound :: Letter)))

getSquare :: Board -> Coords -> Square
getSquare board coords = board!((8 - snd coords))!(letterToInt $ fst coords)

readBoard :: String -> Board
readBoard boardString = fromList $ Data.List.map fromList (Data.List.map (Data.List.map (read . (:[]))) $ lines boardString)

writeBoard :: Board -> String
writeBoard board = (intercalate "\n" $ toList $ Data.Vector.map (toList . Data.Vector.map (Data.List.head . show)) board) Data.List.++ "\n"

makeMove :: Board -> Move -> Board
makeMove board move = imap (\y line ->
  imap (\x square -> replaceSquare board move square (indexesToCoords x y)) line) board

isPlayerPiece :: Color -> Square -> Bool
isPlayerPiece _ Empty = False
isPlayerPiece color (Piece (pieceType, pieceColor)) = pieceColor == color

replaceSquare ::  Board -> Move -> Square -> Coords -> Square
replaceSquare board (origin, destination) square coords
  | coords == origin = Empty
  | coords == destination = getSquare board origin
  | otherwise = square

isMoveAllowed :: Board -> Color -> Move -> Bool
isMoveAllowed board color (origin, destination) = isPlayerPiece color (getSquare board origin)
-- TODO: make this more powerful

gameOver :: Board -> Maybe Color
gameOver board = Nothing
-- TODO: check number of kings on board and conclude

--------
-- IO --
--------

queryMove :: Board -> Color -> IO Move
queryMove board color = do
  moveIO <- getLine
  let move = stringToMove moveIO
  if isMoveAllowed board color move then
    return move
  else do
    putStrLn "Impossible Move. Retry!"
    queryMove board color

gameTurn :: Board -> Color -> IO ()
gameTurn board color = do
  putStr $ boardToAscii board
  putStr ((show color) Data.List.++ "'s turn. Move? (eg. d2d4)\n")
  move <- queryMove board color
  let newBoard = makeMove board move
  let winner = gameOver newBoard
  if winner == Nothing then
    gameTurn newBoard (next color)
  else
    putStr ((show winner) Data.List.++ " won!")

main :: IO ()
main = do
  boardIO <- readFile "board.txt"
  let board = readBoard boardIO
  -- putStr (boardToAscii board)
  gameTurn board White
