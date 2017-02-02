module Main where

import Data.Char
-- [TODO] - Split this file in 2 if possible: 1 w/ Vector & 1 w/ List
import Data.Vector
import Data.List
import System.IO

data PieceType = Pawn | King | Queen | Rook | Bishop | Horse deriving (Show, Eq)
data Color = Black | White deriving (Eq, Show)
type PieceInfo = (PieceType, Color)

-- [TODO] - Create Number class constrained between 1 & 8
type Coords = (Int, Int)
type Move = (Coords, Coords)

data Square = Empty | Piece PieceInfo deriving (Eq)
type Board = Vector (Vector Square)
type GameState = (Board, Color)

instance Show Square where
  show Empty = "."
  show (Piece (pieceType, White)) = [Data.List.head (show pieceType)]
  show (Piece (pieceType, Black)) = [toLower $ Data.List.head (show pieceType)]

instance Read Square where
  readsPrec d ('k': therest) = [(Piece (King, Black), therest)]
  readsPrec d ('q': therest) = [(Piece (Queen, Black), therest)]
  readsPrec d ('r': therest) = [(Piece (Rook, Black), therest)]
  readsPrec d ('b': therest) = [(Piece (Bishop, Black), therest)]
  readsPrec d ('h': therest) = [(Piece (Horse, Black), therest)]
  readsPrec d ('p': therest) = [(Piece (Pawn, Black), therest)]
  readsPrec d ('K': therest) = [(Piece (King, White), therest)]
  readsPrec d ('Q': therest) = [(Piece (Queen, White), therest)]
  readsPrec d ('R': therest) = [(Piece (Rook, White), therest)]
  readsPrec d ('B': therest) = [(Piece (Bishop, White), therest)]
  readsPrec d ('H': therest) = [(Piece (Horse, White), therest)]
  readsPrec d ('P': therest) = [(Piece (Pawn, White), therest)]
  readsPrec d ('.': therest) = [(Empty, therest)]
  readsPrec d _ = []

squareToAscii :: Square -> String
squareToAscii Empty = "."
squareToAscii (Piece (King, Black)) = "♚"
squareToAscii (Piece (Queen, Black)) = "♛"
squareToAscii (Piece (Rook, Black)) = "♜"
squareToAscii (Piece (Bishop, Black)) = "♝"
squareToAscii (Piece (Horse, Black)) = "♞"
squareToAscii (Piece (Pawn, Black)) = "♟"
squareToAscii (Piece (King, White)) = "♔"
squareToAscii (Piece (Queen, White)) = "♕"
squareToAscii (Piece (Rook, White)) = "♖"
squareToAscii (Piece (Bishop, White)) = "♗"
squareToAscii (Piece (Horse, White)) = "♘"
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

chessCoords :: Coords -> String
chessCoords (x, y) = [Data.Char.chr (x+65)] Data.List.++ (show (8 - y))

chessMove :: Move -> String
chessMove (origin, destination) = (chessCoords origin) Data.List.++ "->" Data.List.++ (chessCoords destination)

next :: Color -> Color
next White = Black
next Black = White

-- indexesToCoords :: Int -> Int -> Coords
-- indexesToCoords x y = (read $ (:[]) $ Data.Char.chr ((Data.Char.ord (Data.List.head (show (minBound :: Letter))) + x)), 8 - y)

stringToCoords :: Char -> Char -> Coords
stringToCoords letter number = (x, y)
  where
    x = ((Data.Char.ord $ toUpper letter) - 65) `mod` 8
    y = (8 - read [number]) `mod` 8

stringToMove :: String -> Move
stringToMove (l1:n1:l2:n2:rest) = (stringToCoords l1 n1, stringToCoords l2 n2)

getSquare :: Board -> Coords -> Square
getSquare board (x, y) = board!y!x

readBoard :: String -> Board
readBoard boardString = fromList $ Data.List.map fromList (Data.List.map (Data.List.map (read . (:[]))) $ lines boardString)

writeBoard :: Board -> String
writeBoard board = (intercalate "\n" $ toList $ Data.Vector.map (toList . Data.Vector.map (Data.List.head . show)) board) Data.List.++ "\n"

makeMove :: Board -> Move -> Board
makeMove board move = imap (\y line ->
  imap (\x square -> replaceSquare board move square (x, y)) line) board

isPlayerPiece :: Color -> Square -> Bool
isPlayerPiece _ Empty = False
isPlayerPiece color (Piece (pieceType, pieceColor)) = pieceColor == color

replaceSquare ::  Board -> Move -> Square -> Coords -> Square
replaceSquare board (origin, destination) square coords
  | coords == origin = Empty
  | coords == destination = getSquare board origin
  | otherwise = square

isMoveAllowed :: GameState -> Move -> Bool
isMoveAllowed (board, color) (origin, destination) = isPlayerPiece color (getSquare board origin) && (destination `Data.List.elem` possibleMoves (board, color) origin)

isInBoard :: Coords -> Bool
isInBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

possibleMoves :: GameState -> Coords -> [Coords]
possibleMoves gameState origin =
  case getSquare (fst gameState) origin of
    Empty -> []
    -- Piece (_, next (snd gameState)) -> []
    Piece (King, _) -> (moveInLine gameState origin 1) Data.List.++ (moveInDiagonal gameState origin 1)
    Piece (Queen, _) -> (moveInLine gameState origin 8) Data.List.++ (moveInDiagonal gameState origin 8)
    Piece (Rook, _) -> moveInLine gameState origin 8
    Piece (Bishop, _) -> moveInDiagonal gameState origin 8
    Piece (Horse, _) -> moveHorse gameState origin
    Piece (Pawn, Black) -> movePawn gameState origin (1)
    Piece (Pawn, White) -> movePawn gameState origin (-1)

movePawn :: GameState -> Coords -> Int -> [Coords]
movePawn gameState origin pawnDirection = (movePawnStraight gameState origin pawnDirection) Data.List.++ (movePawnEat gameState origin pawnDirection)

movePawnStraight :: GameState -> Coords -> Int -> [Coords]
movePawnStraight (board, color) (x, y) pawnDirection = Data.List.filter (\coords -> (getSquare board coords) == Empty) ([(x, y+pawnDirection)] Data.List.++ (if y==1 && pawnDirection == 1 || y == 6 && pawnDirection == -1 then [(x, y+2*pawnDirection)] else []))

movePawnEat :: GameState -> Coords -> Int -> [Coords]
movePawnEat (board, color) (x, y) pawnDirection = (
  Data.List.filter (
    \coords -> isPlayerPiece (next color) (getSquare board coords)
  ) [(x+1, y+pawnDirection), (x-1, y+pawnDirection)])

moveInLine :: GameState -> Coords -> Int -> [Coords]
moveInLine gameState (x, y) distance = Data.List.concatMap (checkDirection gameState distance) [(\d -> (x+d, y)), (\d -> (x-d, y)), (\d -> (x, y+d)), (\d -> (x, y-d))]

moveInDiagonal :: GameState -> Coords -> Int -> [Coords]
moveInDiagonal gameState (x, y) distance =  Data.List.concatMap (checkDirection gameState distance) [(\ d -> (x+d, y+d)), (\ d -> (x+d, y-d)), (\ d -> (x-d, y-d)), (\ d -> (x-d, y+d))]

moveHorse :: GameState -> Coords -> [Coords]
moveHorse (board, color) (x, y) =  Data.List.filter (\coords ->
  let square = getSquare board coords in
  (isInBoard coords) && (not (isPlayerPiece color square))
  ) [(x+2, y+1), (x+1, y+2), (x+2, y-1), (x+1, y-2), (x-2, y-1), (x-1, y-2), (x-2, y+1), (x-1, y+2)]

canContinueInDirection :: GameState -> Coords -> Maybe Bool
canContinueInDirection (board, color) coords
  | (not . isInBoard $ coords) || (isPlayerPiece color square) = Nothing -- Stop scanning
  | otherwise = Just (square == Empty) -- Empty: keep scanning, opponent piece: stop but keep move as possible
  where
    square = getSquare board coords

checkDirection :: GameState -> Int -> (Int -> Coords) -> [Coords]
checkDirection gameState distance transformator = Data.List.foldr (\coords ys ->
  case canContinueInDirection gameState coords of
    Nothing -> []
    Just False -> [coords]
    Just True -> (coords:ys)
  ) [] $ Data.List.map transformator [1..distance]


isKing :: Square -> Bool
isKing (Piece (piecetype, _)) = piecetype == King
isKing Empty = False

getColor :: Square -> Maybe Color
getColor (Piece (_, color)) = Just color
getColor Empty = Nothing

gameOverWinner :: Board -> Maybe Color
gameOverWinner board = 
  let kings = Data.Vector.concatMap (Data.Vector.filter isKing) board
  in if Data.Vector.length kings == 2 then Nothing else getColor (Data.Vector.head kings)

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
    putStrLn ("Possible Moves:" Data.List.++ Data.List.concatMap chessCoords (possibleMoves gameState (fst move)))
    queryMove gameState

gameTurn :: GameState -> IO ()
gameTurn (board, color) = do
  putStr $ boardToAscii board
  putStr ((show color) Data.List.++ "'s turn. Move? (eg. d2d4)\n")
  move <- queryMove (board, color)
  let newBoard = makeMove board move
  let winner = gameOverWinner newBoard
  if winner == Nothing then
    gameTurn (newBoard, next color)
  else
    putStr ((show (fromJust winner)) Data.List.++ " won!")

main :: IO ()
main = do
  boardIO <- readFile "board.txt"
  let board = readBoard boardIO
  -- putStr (boardToAscii board)
  gameTurn (board, White)
