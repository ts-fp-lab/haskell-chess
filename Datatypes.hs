module Datatypes where
  import Data.Vector
  import Data.List
  import Data.Char

  data PieceType = Pawn | King | Queen | Rook | Bishop | Horse deriving (Show, Eq)
  data Color = Black | White deriving (Eq, Show)
  type PieceInfo = (PieceType, Color)

  -- [TODO] - Create Number class constrained between 1 & 8
  type Coords = (Int, Int)
  type Move = (Coords, Coords)

  data Square = Empty | Piece PieceInfo deriving (Eq)

  instance Show Square where
    show Empty = "."
    show (Piece (pieceType, White)) = [Data.List.head (show pieceType)]
    show (Piece (pieceType, Black)) = [toLower $ Data.List.head (show pieceType)]

  type Board = Vector (Vector Square)
  type GameState = (Board, Color)

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


