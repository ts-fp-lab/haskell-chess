module Rules where
  import Datatypes
  import Data.List as L
  import Data.Vector as V
  import Data.Char
  import Board

  next :: Color -> Color
  next White = Black
  next Black = White

  chessCoords :: Coords -> String
  chessCoords (x, y) = [Data.Char.chr (x+65)] L.++ (show (8 - y))

  chessMove :: Move -> String
  chessMove (origin, destination) = (chessCoords origin) L.++ "->" L.++ (chessCoords destination)

  isInBoard :: Coords -> Bool
  isInBoard (x, y) = x >= 0 && x < 8 && y >= 0 && y < 8


  isPlayerPiece :: Color -> Square -> Bool
  isPlayerPiece _ Empty = False
  isPlayerPiece color (Piece (pieceType, pieceColor)) = pieceColor == color

  moveReturnsError :: GameState -> Move -> Maybe String
  moveReturnsError (board, color) (origin, destination)
    | not (isPlayerPiece color square) = Just $ "Not your piece: " L.++ show square
    | not (destination `L.elem` possibleDestinations) = Just $ "Impossible move from " L.++ (show square) L.++"\nPossible moves=" L.++ L.concatMap chessCoords possibleDestinations
    | otherwise = Nothing
    where
      square = getSquare board origin
      possibleDestinations = possibleDestinationsFromOrigin (board, color) origin

  -- TODO: if check then possible moves change
  possibleDestinationsFromOrigin :: GameState -> Coords -> [Coords]
  possibleDestinationsFromOrigin (board, color) origin =
    case getSquare board origin of
      Empty -> []
      Piece (pieceType, pieceColor) ->
        if pieceColor == color then possibleDestinationsPerType (board, color) origin pieceType pieceColor
        else []

  possibleDestinationsPerType :: GameState -> Coords -> PieceType -> Color -> [Coords]
  possibleDestinationsPerType gameState origin King _ = (moveInLine gameState origin 1) L.++ (moveInDiagonal gameState origin 1)
  possibleDestinationsPerType gameState origin Queen _ = (moveInLine gameState origin 8) L.++ (moveInDiagonal gameState origin 8)
  possibleDestinationsPerType gameState origin Rook _ = moveInLine gameState origin 8
  possibleDestinationsPerType gameState origin Bishop _ = moveInDiagonal gameState origin 8
  possibleDestinationsPerType gameState origin Horse _ = moveHorse gameState origin
  possibleDestinationsPerType gameState origin Pawn White = movePawn gameState origin (-1)
  possibleDestinationsPerType gameState origin Pawn Black = movePawn gameState origin (1)

  possibleMovesFromOrigin :: Coords -> [Coords] -> [Move]
  possibleMovesFromOrigin origin [] = []
  possibleMovesFromOrigin origin (dest:rest) = ((origin, dest):(possibleMovesFromOrigin origin rest))

  getListFromCoords :: GameState -> Coords -> [Move]
  getListFromCoords gameState coords = possibleMovesFromOrigin coords (possibleDestinationsFromOrigin gameState coords)

  boardPossibleMoves :: GameState -> [Move]
  boardPossibleMoves gameState =
    let listMatrix = V.imap (\i line -> V.imap (\j square -> getListFromCoords gameState (i,j)) line) (fst gameState) in
      L.concat . L.concat $ matrixToLists listMatrix

  movePawn :: GameState -> Coords -> Int -> [Coords]
  movePawn gameState origin pawnDirection = (movePawnStraight gameState origin pawnDirection) L.++ (movePawnEat gameState origin pawnDirection)

  movePawnStraight :: GameState -> Coords -> Int -> [Coords]
  movePawnStraight (board, color) (x, y) pawnDirection = L.filter (\coords -> (getSquare board coords) == Empty) ([(x, y+pawnDirection)] L.++ (if y==1 && pawnDirection == 1 || y == 6 && pawnDirection == -1 then [(x, y+2*pawnDirection)] else []))

  movePawnEat :: GameState -> Coords -> Int -> [Coords]
  movePawnEat (board, color) (x, y) pawnDirection = (
    L.filter (
      \coords -> isPlayerPiece (next color) (getSquare board coords)
    ) [(x+1, y+pawnDirection), (x-1, y+pawnDirection)])

  moveInLine :: GameState -> Coords -> Int -> [Coords]
  moveInLine gameState (x, y) distance = L.concatMap (checkDirection gameState distance) [(\d -> (x+d, y)), (\d -> (x-d, y)), (\d -> (x, y+d)), (\d -> (x, y-d))]

  moveInDiagonal :: GameState -> Coords -> Int -> [Coords]
  moveInDiagonal gameState (x, y) distance =  L.concatMap (checkDirection gameState distance) [(\ d -> (x+d, y+d)), (\ d -> (x+d, y-d)), (\ d -> (x-d, y-d)), (\ d -> (x-d, y+d))]

  moveHorse :: GameState -> Coords -> [Coords]
  moveHorse (board, color) (x, y) =  L.filter (\coords ->
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
  checkDirection gameState distance transformator = L.foldr (\coords ys ->
    case canContinueInDirection gameState coords of
      Nothing -> []
      Just False -> [coords]
      Just True -> (coords:ys)
    ) [] $ L.map transformator [1..distance]