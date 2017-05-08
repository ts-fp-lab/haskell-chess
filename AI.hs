module AI where

  import Datatypes
  import Board
  import Rules



  getMove :: GameState -> Move
  getMove gameState = head $ boardPossibleMoves gameState
