module AI where
  import Datatypes
  import Board
  import Rules

  import Data.List
  -- -- TODO: replace by official function that MUST exist
  myMinimumBy :: (a -> Int) -> [a] -> a
  myMinimumBy fn [a] = a
  myMinimumBy fn (a:b) = if (fn a) < (fn minRest) then a else minRest
    where
      minRest = myMinimumBy fn b


  getMove :: GameState -> Int -> Move
  getMove gameState seed = getMoveMinMax gameState

  -- TODO6: Minmax algorithm

  getMoveRandom :: GameState -> Int -> Move
  getMoveRandom gameState seed = possibleMoves!!index
    where
      possibleMoves = boardPossibleMoves gameState
      index = seed `mod` (length possibleMoves)

  getMoveMinMax :: GameState -> Move
  getMoveMinMax (board, color) = bestMove
    where
      possibleMoves = boardPossibleMoves (board, color)
      possibleBoards = map (\move -> makeMove board move) possibleMoves
      bestMove = myMinimumBy (moveValue (board, color)) possibleMoves

  moveValue :: GameState -> Move -> Int
  moveValue (board, color) move = boardHeuristic (makeMove board move, next color)

  -- The heuristic depends on:
  --  - points Diff on board
  --  - nb of square threat

  boardHeuristic :: GameState -> Int
  boardHeuristic state = (onBoardPieceValues state) + (squaresUnderControl state)

  pieceValue :: PieceType -> Int
  pieceValue Queen = 10
  pieceValue Rook = 5
  pieceValue Bishop = 3
  pieceValue Horse = 3
  pieceValue Pawn = 1
  pieceValue _ = 0

  colorFactor :: Color -> Color -> Int
  colorFactor col1 col2
    | col1 == col2 = 1
    | otherwise = 0

  squareValue :: Square -> Color -> Int
  squareValue Empty playerColor = 0
  squareValue (Piece (pieceType, pieceColor)) playerColor = (pieceValue pieceType) * (colorFactor pieceColor playerColor)

  onBoardPieceValues :: GameState -> Int
  onBoardPieceValues (board, color) =
    reduceBoard (\coords square -> squareValue square color) (+) 0 board

  squaresUnderControl :: GameState -> Int
  squaresUnderControl (board, color) =
    reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin (board, color) coords) (+) 0 board