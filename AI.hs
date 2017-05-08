module AI where

  import Datatypes
  import Board
  import Rules



  getMove :: GameState -> Int -> Move
  getMove gameState seed = possibleMoves!!index
  -- getMove gameState seed = head possibleMoves
    where
      possibleMoves = boardPossibleMoves gameState
      index = seed `mod` (length possibleMoves)
