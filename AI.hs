module AI where
  import Datatypes
  import Board
  import Rules

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
      possibleMoves = statePossibleMoves state
      index = seed `mod` (length possibleMoves)

  getBestMoveNoRecursion :: GameState -> Move
  getBestMoveNoRecursion state = bestMove
    where
      possibleMoves = statePossibleMoves state
      bestMove = myMinimumBy (moveValue state) possibleMoves

  getBestMoveMinMax :: GameState -> Move
  getBestMoveMinMax state = minimax 2 state



  -- Code inspired from the excellent http://giocc.com/concise-implementation-of-minimax-through-higher-order-functions.html

  foldMoves :: Int -> GameState -> (Move, Int) -> Move -> (Move, Int)
  foldMoves depth state (currentBestMove, currentBestScore) move
    | nextScore > currentBestScore = (move, nextScore)
    | otherwise = (currentBestMove, currentBestScore)
    where
      nextScore = maxPlay depth (makeMove state move)

  -- negamax :: Int -> GameState -> Move
  -- negamax depth (board, color) =
  --   if depth == 0
  --     then boardHeuristic (board, color)
  --     else

  minimax :: Int -> GameState -> Move
  minimax depth state = bestMove
    where
      moves = statePossibleMoves state
      firstPossibleMove = head moves
      firstPossibleScore = boardHeuristic (makeMove state firstPossibleMove)
      (bestMove, bestScore) = foldl (foldMoves depth state) (firstPossibleMove, firstPossibleScore) moves


  minPlay :: Int -> GameState -> Int
  minPlay 0 state = boardHeuristic state
  minPlay depth state = minimum nextScores
    where
      nextMoves = statePossibleMoves state
      nextStates = map (makeMove state) nextMoves
      nextScores = map (maxPlay (depth-1)) nextStates


  maxPlay :: Int -> GameState -> Int
  maxPlay 0 state = boardHeuristic state
  maxPlay depth state = maximum nextScores
    where
      nextMoves = statePossibleMoves state
      nextStates = map (makeMove state) nextMoves
      nextScores = map (minPlay (depth-1)) nextStates

  -- The heuristic depends on:
  --  - points Diff on board
  --  - nb of square threat

  moveValue :: GameState -> Move -> Int
  moveValue state move = boardHeuristic (makeMove state move)


  boardHeuristic :: GameState -> Int
  boardHeuristic (board, color) = 0
    + (onBoardPieceValuesAndPosition board color)
    -- + (squaresUnderControl startColor state)

  -- From this article: https://webcache.googleusercontent.com/search?q=cache:Umd_gBjDTskJ:https://chessprogramming.wikispaces.com/Simplified%2Bevaluation%2Bfunction+&cd=1&hl=en&ct=clnk&gl=fr&client=ubuntu
  pieceValue :: PieceType -> Int
  pieceValue King = 20000
  pieceValue Queen = 900
  pieceValue Rook = 500
  pieceValue Bishop = 330
  pieceValue Horse = 320
  pieceValue Pawn = 100

  colorFactor :: Color -> Color -> Int
  colorFactor col1 col2
    | col1 == col2 = 1
    | otherwise = -1

  pieceTableBonus :: PieceType -> Coords -> Int
  pieceTableBonus pieceType coords = 0

  squareValue :: Color -> Coords -> Square -> Int
  squareValue _ _ Empty = 0
  squareValue color coords (Piece (pieceType, pieceColor)) = ((pieceValue pieceType) + (pieceTableBonus pieceType coords)) * (colorFactor pieceColor color)

  onBoardPieceValuesAndPosition :: Board -> Color -> Int
  onBoardPieceValuesAndPosition board color =
    reduceBoard (squareValue color) (+) 0 board

  -- squaresUnderControl :: Color -> GameState -> Int
  -- squaresUnderControl startColor (board, color) = playerControl - opponentControl
  --   where
  --   playerControl = reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin board startColor coords square) (+) 0 board
  --   opponentControl = reduceBoard (\coords square -> length $ possibleDestinationsFromOrigin board (next startColor) coords square) (+) 0 board