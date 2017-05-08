module AI where

  import Datatypes
  import Board
  import Rules


  -- TODO6: Minmax algorithm

  getMove :: GameState -> Int -> Move
  getMove gameState seed = possibleMoves!!index
  -- getMove gameState seed = head possibleMoves
    where
      possibleMoves = boardPossibleMoves gameState
      index = seed `mod` (length possibleMoves)
