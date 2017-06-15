module AI where
  import Datatypes
  import Board
  import Rules
  import Heuristics

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
      (board, color) = state
      possibleMoves = statePossibleMoves state
      bestMove = myMinimumBy (moveValue board color) possibleMoves

  getBestMoveMinMax :: GameState -> Move
  getBestMoveMinMax state = negamax 2 state



  -- Code inspired from the excellent http://giocc.com/concise-implementation-of-minimax-through-higher-order-functions.html
  -- and from https://webcache.googleusercontent.com/search?q=cache:ixWFqj2T1CQJ:https://chessprogramming.wikispaces.com/Negamax+&cd=1&hl=en&ct=clnk&gl=fr

  foldMoves :: Int -> GameState -> (Move, Int) -> Move -> (Move, Int)
  foldMoves depth state (currentBestMove, currentBestScore) move
    | nextScore > currentBestScore = (move, nextScore)
    | otherwise = (currentBestMove, currentBestScore)
    where
      nextScore = -negamaxScore depth (makeMove state move)

  negamax :: Int -> GameState -> Move
  negamax depth state = bestMove
    where
      nextMoves = statePossibleMoves state
      (bestMove, bestScore) = foldl (foldMoves depth state) (head nextMoves, minBound :: Int) nextMoves

  negamaxScore :: Int -> GameState -> Int
  negamaxScore 0 (board, color) = boardHeuristic board color
  negamaxScore depth state = maximum $ map (\move -> -(negamaxScore (depth -1) (makeMove state move))) nextMoves
    where
      nextMoves = statePossibleMoves state

  -- alphaBetaScore :: Int -> Int -> Int -> GameState -> Int
  -- alphaBetaScore _ _ 0 (board, color) = boardHeuristic board color
  -- alphaBetaScore alphaInit betaInit depth state =
  --   foldl (\(alpha, beta) move ->
  --     let deeperScore = -(alphaBetaScore alpha beta (depth -1) (makeMove state move)) in
  --     (minimum [alpha, deeperScore], maximum [beta, deeperScore])
  --   ) (alphaInit, betaInit) nextMoves
  --   where
  --     nextMoves = statePossibleMoves state

  -- quiesce :: Int -> Int -> GameState -> Int
  -- quiesce alpha beta (board, color) =
  --   let heuristic = boardHeuristic board color in
  --   let captures = statePossibleCaptures state in
  --   captures