module Heuristics where
  import Data.Vector

  import Datatypes
  import Board

  -- Taken from there: https://webcache.googleusercontent.com/search?q=cache:Umd_gBjDTskJ:https://chessprogramming.wikispaces.com/Simplified%2Bevaluation%2Bfunction+&cd=1&hl=en&ct=clnk&gl=fr&client=ubuntu

  moveValue :: Board -> Color -> Move -> Int
  moveValue board color move = boardHeuristic (boardMove board move) color

  boardHeuristic :: Board -> Color -> Int
  boardHeuristic board color = 0
    + (onBoardPieceValuesAndPosition board color)
    -- + (squaresUnderControl startColor state)

  colorFactor :: Color -> Color -> Int
  colorFactor col1 col2
    | col1 == col2 = 1
    | otherwise = -1


  onBoardPieceValuesAndPosition :: Board -> Color -> Int
  onBoardPieceValuesAndPosition board color =
    reduceBoard (squareValue color) (+) 0 board

  squareValue :: Color -> Coords -> Square -> Int
  squareValue _ _ Empty = 0
  squareValue color coords (Piece (pieceType, pieceColor)) = ((pieceValue pieceType) + (pieceTypeTableValue pieceType pieceColor coords)) * (colorFactor pieceColor color)

  -- squaresUnderControl :: Color -> GameState -> Int
  -- squaresUnderControl startColor (board, color) = playerControl - opponentControl
  --   where
  --   playerControl = reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin board startColor coords square) (+) 0 board
  --   opponentControl = reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin board (next startColor) coords square) (+) 0 board

  pieceValue :: PieceType -> Int
  pieceValue King = 20000
  pieceValue Queen = 900
  pieceValue Rook = 500
  pieceValue Bishop = 330
  pieceValue Horse = 320
  pieceValue Pawn = 100

  pieceTypeTableValue :: PieceType -> Color -> Coords -> Int
  pieceTypeTableValue pieceType pieceColor (x, y) = (pieceTypeTable pieceType)!(if pieceColor == White then y else (7-y))!x

  pieceTypeTable :: PieceType -> Vector (Vector Int)
  pieceTypeTable Pawn = fromList [
    fromList [ 0,  0,  0,  0,  0,  0,  0,  0],
    fromList [50, 50, 50, 50, 50, 50, 50, 50],
    fromList [10, 10, 20, 30, 30, 20, 10, 10],
    fromList [ 5,  5, 10, 25, 25, 10,  5,  5],
    fromList [ 0,  0,  0, 20, 20,  0,  0,  0],
    fromList [ 5, -5,-10,  0,  0,-10, -5,  5],
    fromList [ 5, 10, 10,-20,-20, 10, 10,  5],
    fromList [ 0,  0,  0,  0,  0,  0,  0,  0]]

  pieceTypeTable Horse = fromList [
    fromList [-50,-40,-30,-30,-30,-30,-40,-50],
    fromList [-40,-20,  0,  0,  0,  0,-20,-40],
    fromList [-30,  0, 10, 15, 15, 10,  0,-30],
    fromList [-30,  5, 15, 20, 20, 15,  5,-30],
    fromList [-30,  0, 15, 20, 20, 15,  0,-30],
    fromList [-30,  5, 10, 15, 15, 10,  5,-30],
    fromList [-40,-20,  0,  5,  5,  0,-20,-40],
    fromList [-50,-40,-30,-30,-30,-30,-40,-50]]

  pieceTypeTable Bishop = fromList [
    fromList [-20,-10,-10,-10,-10,-10,-10,-20],
    fromList [-10,  0,  0,  0,  0,  0,  0,-10],
    fromList [-10,  0,  5, 10, 10,  5,  0,-10],
    fromList [-10,  5,  5, 10, 10,  5,  5,-10],
    fromList [-10,  0, 10, 10, 10, 10,  0,-10],
    fromList [-10, 10, 10, 10, 10, 10, 10,-10],
    fromList [-10,  5,  0,  0,  0,  0,  5,-10],
    fromList [-20,-10,-10,-10,-10,-10,-10,-20]]

  pieceTypeTable Rook = fromList [
    fromList [ 0,  0,  0,  0,  0,  0,  0,  0],
    fromList [ 5, 10, 10, 10, 10, 10, 10,  5],
    fromList [-5,  0,  0,  0,  0,  0,  0, -5],
    fromList [-5,  0,  0,  0,  0,  0,  0, -5],
    fromList [-5,  0,  0,  0,  0,  0,  0, -5],
    fromList [-5,  0,  0,  0,  0,  0,  0, -5],
    fromList [-5,  0,  0,  0,  0,  0,  0, -5],
    fromList [ 0,  0,  0,  5,  5,  0,  0,  0]]

  pieceTypeTable Queen = fromList [
    fromList [-20,-10,-10, -5, -5,-10,-10,-20],
    fromList [-10,  0,  0,  0,  0,  0,  0,-10],
    fromList [-10,  0,  5,  5,  5,  5,  0,-10],
    fromList [ -5,  0,  5,  5,  5,  5,  0, -5],
    fromList [  0,  0,  5,  5,  5,  5,  0, -5],
    fromList [-10,  5,  5,  5,  5,  5,  0,-10],
    fromList [-10,  0,  5,  0,  0,  0,  0,-10],
    fromList [-20,-10,-10, -5, -5,-10,-10,-20]]

  pieceTypeTable King = fromList [
    fromList [-50,-40,-30,-30,-30,-30,-40,-50],
    fromList [-40,-20,  0,  0,  0,  0,-20,-40],
    fromList [-30,  0, 10, 15, 15, 10,  0,-30],
    fromList [-30,  5, 15, 20, 20, 15,  5,-30],
    fromList [-30,  0, 15, 20, 20, 15,  0,-30],
    fromList [-30,  5, 10, 15, 15, 10,  5,-30],
    fromList [-40,-20,  0,  5,  5,  0,-20,-40],
    fromList [-50,-40,-30,-30,-30,-30,-40,-50]]

