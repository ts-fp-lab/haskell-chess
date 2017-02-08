module Rules where
  import Datatypes
  import Data.List
  import Board

  next :: Color -> Color
  next White = Black
  next Black = White



  isPlayerPiece :: Color -> Square -> Bool
  isPlayerPiece _ Empty = False
  isPlayerPiece color (Piece (pieceType, pieceColor)) = pieceColor == color

  isMoveAllowed :: GameState -> Move -> Bool
  isMoveAllowed (board, color) (origin, destination) = isPlayerPiece color (getSquare board origin) && (destination `Data.List.elem` possibleMoves (board, color) origin)

  isInBoard :: Coords -> Bool
  isInBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8

  possibleMoves :: GameState -> Coords -> [Coords]
  possibleMoves gameState origin =
    case getSquare (fst gameState) origin of
      Empty -> []
      -- Piece (_, next (snd gameState)) -> []
      Piece (King, _) -> (moveInLine gameState origin 1) Data.List.++ (moveInDiagonal gameState origin 1)
      Piece (Queen, _) -> (moveInLine gameState origin 8) Data.List.++ (moveInDiagonal gameState origin 8)
      Piece (Rook, _) -> moveInLine gameState origin 8
      Piece (Bishop, _) -> moveInDiagonal gameState origin 8
      Piece (Horse, _) -> moveHorse gameState origin
      Piece (Pawn, Black) -> movePawn gameState origin (1)
      Piece (Pawn, White) -> movePawn gameState origin (-1)

  movePawn :: GameState -> Coords -> Int -> [Coords]
  movePawn gameState origin pawnDirection = (movePawnStraight gameState origin pawnDirection) Data.List.++ (movePawnEat gameState origin pawnDirection)

  movePawnStraight :: GameState -> Coords -> Int -> [Coords]
  movePawnStraight (board, color) (x, y) pawnDirection = Data.List.filter (\coords -> (getSquare board coords) == Empty) ([(x, y+pawnDirection)] Data.List.++ (if y==1 && pawnDirection == 1 || y == 6 && pawnDirection == -1 then [(x, y+2*pawnDirection)] else []))

  movePawnEat :: GameState -> Coords -> Int -> [Coords]
  movePawnEat (board, color) (x, y) pawnDirection = (
    Data.List.filter (
      \coords -> isPlayerPiece (next color) (getSquare board coords)
    ) [(x+1, y+pawnDirection), (x-1, y+pawnDirection)])

  moveInLine :: GameState -> Coords -> Int -> [Coords]
  moveInLine gameState (x, y) distance = Data.List.concatMap (checkDirection gameState distance) [(\d -> (x+d, y)), (\d -> (x-d, y)), (\d -> (x, y+d)), (\d -> (x, y-d))]

  moveInDiagonal :: GameState -> Coords -> Int -> [Coords]
  moveInDiagonal gameState (x, y) distance =  Data.List.concatMap (checkDirection gameState distance) [(\ d -> (x+d, y+d)), (\ d -> (x+d, y-d)), (\ d -> (x-d, y-d)), (\ d -> (x-d, y+d))]

  moveHorse :: GameState -> Coords -> [Coords]
  moveHorse (board, color) (x, y) =  Data.List.filter (\coords ->
    let square = getSquare board coords in
    (isInBoard coords) && (not (isPlayerPiece color square))
    ) [(x+2, y+1), (x+1, y+2), (x+2, y-1), (x+1, y-2), (x-2, y-1), (x-1, y-2), (x-2, y+1), (x-1, y+2)]

  canContinueInDirection :: GameState -> Coords -> Maybe Bool
  canContinueInDirection (board, color) coords
    | (not . isInBoard $ coords) || (isPlayerPiece color square) = Nothing -- Stop scanning
    | otherwise = Just (square == Empty) -- Empty: keep scanning, opponent piece: stop but keep move as possible
    where
      square = getSquare board coords

  checkDirection :: GameState -> Int -> (Int -> Coords) -> [Coords]
  checkDirection gameState distance transformator = Data.List.foldr (\coords ys ->
    case canContinueInDirection gameState coords of
      Nothing -> []
      Just False -> [coords]
      Just True -> (coords:ys)
    ) [] $ Data.List.map transformator [1..distance]