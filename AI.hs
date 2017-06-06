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
      bestMove = myMinimumBy (moveValue (snd state) state) possibleMoves

  getBestMoveMinMax :: GameState -> Move
  getBestMoveMinMax state = minimax 2 (snd state) state



  -- Code inspired from the excellent http://giocc.com/concise-implementation-of-minimax-through-higher-order-functions.html

  foldMoves :: Int -> Color -> GameState -> (Move, Int) -> Move -> (Move, Int)
  foldMoves depth startColor state (currentBestMove, currentBestScore) move
    | nextScore > currentBestScore = (move, nextScore)
    | otherwise = (currentBestMove, currentBestScore)
    where
      nextScore = maxPlay depth startColor (makeMove state move)

  minimax :: Int -> Color -> GameState -> Move
  minimax depth startColor state = bestMove
    where
      moves = boardPossibleMoves state
      firstPossibleMove = head moves
      firstPossibleScore = boardHeuristic startColor (makeMove state firstPossibleMove)
      (bestMove, bestScore) = foldl (foldMoves depth startColor state) (firstPossibleMove, firstPossibleScore) moves


  minPlay :: Int -> Color -> GameState -> Int
  minPlay 0 startColor state = boardHeuristic startColor state
  minPlay depth startColor state = minimum nextScores
    where
      nextMoves = boardPossibleMoves state
      nextStates = map (makeMove state) nextMoves
      nextScores = map (maxPlay (depth-1) startColor) nextStates


  maxPlay :: Int -> Color -> GameState -> Int
  maxPlay 0 startColor state = boardHeuristic startColor state
  maxPlay depth startColor state = maximum nextScores
    where
      nextMoves = boardPossibleMoves state
      nextStates = map (makeMove state) nextMoves
      nextScores = map (minPlay (depth-1) startColor) nextStates

  -- The heuristic depends on:
  --  - points Diff on board
  --  - nb of square threat

  moveValue :: Color -> GameState -> Move -> Int
  moveValue startColor state move = boardHeuristic startColor (makeMove state move)


  boardHeuristic :: Color -> GameState -> Int
  boardHeuristic startColor state = 0
    + 10 * (onBoardPieceValues startColor state)
    + 1 * (squaresUnderControl startColor state)

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
    | otherwise = -1

  squareValue :: Square -> Color -> Int
  squareValue Empty playerColor = 0
  squareValue (Piece (pieceType, pieceColor)) playerColor = (pieceValue pieceType) * (colorFactor pieceColor playerColor)

  onBoardPieceValues :: Color -> GameState -> Int
  onBoardPieceValues startColor (board, color) =
    reduceBoard (\coords square -> squareValue square startColor) (+) 0 board

  squaresUnderControl :: Color -> GameState -> Int
  squaresUnderControl startColor (board, color) = playerControl - opponentControl
    where
    playerControl = reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin board startColor coords) (+) 0 board
    opponentControl = reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin board (next startColor) coords) (+) 0 board