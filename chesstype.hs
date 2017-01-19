import Data.Char
import Data.Vector
import Data.List

data PieceType = Pawn | King | Queen | Rook | Bishop | Knight deriving (Show)
data Letter = A | B | C | D | E | F | G | H deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- data Number = One | Two | Three | Four | Five | Six | Seven | Eight deriving (Eq, Ord, Show, Read, Bounded, Enum)
data Color = Black | White
type PieceInfo = (PieceType, Color)
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

-- instance Read Square where
--   readsPrec d ('k': therest) = [(Piece (King, Black), therest)]
--   readsPrec d ('q': therest) = [(Piece (Queen, Black), therest)]
--   readsPrec d ('r': therest) = [(Piece (Rook, Black), therest)]
--   readsPrec d ('b': therest) = [(Piece (Bishop, Black), therest)]
--   readsPrec d ('n': therest) = [(Piece (Knight, Black), therest)]
--   readsPrec d ('p': therest) = [(Piece (Pawn, Black), therest)]
--   readsPrec d ('K': therest) = [(Piece (King, White), therest)]
--   readsPrec d ('Q': therest) = [(Piece (Queen, White), therest)]
--   readsPrec d ('R': therest) = [(Piece (Rook, White), therest)]
--   readsPrec d ('B': therest) = [(Piece (Bishop, White), therest)]
--   readsPrec d ('N': therest) = [(Piece (Knight, White), therest)]
--   readsPrec d ('P': therest) = [(Piece (Pawn, White), therest)]
--   readsPrec d ('.': therest) = [(Empty, therest)]
--   readsPrec d _ = []

instance Read Square where


indexesToCoords :: Int -> Int -> Coords
indexesToCoords x y = (read $ (:[]) $ Data.Char.chr ((Data.Char.ord (Data.List.head (show (minBound :: Letter))) + x)), 8 - y)

readBoard :: String -> Board
readBoard boardString = fromList $ Data.List.map fromList (Data.List.map (Data.List.map (read . (:[]))) $ lines boardString)

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
boardLineToAscii nb line = (show nb) Data.List.++ " ║" Data.List.++ (Data.List.concatMap squareToAscii (toList line)) Data.List.++ "║ "  Data.List.++ (show nb)

boardToAscii :: Board -> String
boardToAscii board = unlines (toList (Data.Vector.imap (\lineNb line -> boardLineToAscii lineNb line) board))

charToSquare :: Char -> Square
charToSquare c = read [c]

isInBoard :: (Int, Int) -> Bool
isInBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8