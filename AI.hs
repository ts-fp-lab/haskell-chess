module AI where
  import Datatypes
  import Board
  import Rules

  -- import Data.List
  -- TODO: replace by official function
  myMinimumBy :: (a -> Int) -> [a] -> a
  myMinimumBy fn [a] = a
  myMinimumBy fn (a:b) = if (fn a) < (fn minRest) then a else minRest
    where
      minRest = myMinimumBy fn b



  getMove :: GameState -> Int -> Move
  getMove state seed = getBestMoveMinMax state


  getRandomMove :: GameState -> Int -> Move
  getRandomMove state seed = possibleMoves!!index
    where
      possibleMoves = boardPossibleMoves state
      index = seed `mod` (length possibleMoves)

  getBestMoveNoRecursion :: GameState -> Move
  getBestMoveNoRecursion state = bestMove
    where
      possibleMoves = boardPossibleMoves state
      bestMove = myMinimumBy (moveValue state) possibleMoves

  getBestMoveMinMax :: GameState -> Move
  getBestMoveMinMax state = minimax state 1



  -- Code inspired from the excellent http://giocc.com/concise-implementation-of-minimax-through-higher-order-functions.html

  foldMoves :: GameState -> Int -> (Move, Int) -> Move -> (Move, Int)
  foldMoves state depth (currentBestMove, currentBestScore) move
    | nextScore > currentBestScore = (move, nextScore)
    | otherwise = (currentBestMove, currentBestScore)
    where
      nextScore = minPlay depth (makeMove state move)

  minimax :: GameState -> Int -> Move
  minimax state depth = bestMove
    where
      moves = boardPossibleMoves state
      firstPossibleMove = head moves
      firstPossibleScore = boardHeuristic (makeMove state firstPossibleMove)
      (bestMove, bestScore) = foldl (foldMoves state depth) (firstPossibleMove, firstPossibleScore) moves


  minPlay :: Int -> GameState -> Int
  minPlay 0 state = boardHeuristic state
  minPlay depth state = minimum nextScores
    where
      nextMoves = boardPossibleMoves state
      nextStates = map (makeMove state) nextMoves
      nextScores = map (maxPlay depth) nextStates


  maxPlay :: Int -> GameState -> Int
  maxPlay 0 state = boardHeuristic state
  maxPlay depth state = maximum nextScores
    where
      nextMoves = boardPossibleMoves state
      nextStates = map (makeMove state) nextMoves
      nextScores = map (minPlay (depth-1)) nextStates


  moveValue :: GameState -> Move -> Int
  moveValue state move = boardHeuristic (makeMove state move)

  -- The heuristic depends on:
  --  - points Diff on board
  --  - nb of square threat

  boardHeuristic :: GameState -> Int
  boardHeuristic state = 0 +
    100 * (onBoardPieceValues state) +
    1 * (squaresUnderControl state)

  pieceValue :: PieceType -> Int
  pieceValue Queen = 10
  pieceValue Rook = 5
  pieceValue Bishop = 3
  pieceValue Horse = 3
  pieceValue Pawn = 1
  pieceValue _ = 0

  colorFactor :: Color -> Color -> Int
  colorFactor col1 col2
    | col1 == col2 = -1
    | otherwise = 1

  squareValue :: Square -> Color -> Int
  squareValue Empty playerColor = 0
  squareValue (Piece (pieceType, pieceColor)) playerColor = (pieceValue pieceType) * (colorFactor pieceColor playerColor)

  onBoardPieceValues :: GameState -> Int
  onBoardPieceValues (board, color) =
    reduceBoard (\coords square -> squareValue square color) (+) 0 board

  squaresUnderControl :: GameState -> Int
  squaresUnderControl (board, color) =
    reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin (board, color) coords) (+) 0 board