module Rules where
  import Datatypes
  import Data.List as L
  import Data.Vector as V
  import Data.Char
  import Board
  import Data.Maybe

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
      possibleDestinations = possibleDestinationsFromOrigin board color origin square


  -- TODO: Add castling
  basicPieceTypeMover :: PieceType -> (Board -> Color -> Coords -> [Coords])
  basicPieceTypeMover King = moveInAllDirections 1
  basicPieceTypeMover Queen = moveInAllDirections 8
  basicPieceTypeMover Rook = moveInLine 8
  basicPieceTypeMover Bishop = moveInDiagonal 8
  basicPieceTypeMover Horse = moveHorse
  basicPieceTypeMover Pawn = movePawn

  movePieceAndRemoveCheck :: PieceType -> Board -> Color -> Coords -> [Coords]
  movePieceAndRemoveCheck pieceType board color origin = removeCheckPosition board color origin $ basicPieceTypeMover pieceType board color origin

  possibleDestinationsFromOriginNoMover :: (PieceType -> Board -> Color -> Coords -> [Coords]) -> Board -> Color -> Coords -> Square -> [Coords]
  possibleDestinationsFromOriginNoMover mover board color origin square =
    case square of
      Empty -> []
      Piece (pieceType, pieceColor) -> if color == pieceColor
        then mover pieceType board pieceColor origin
        else []

  possibleDestinationsFromOrigin :: Board -> Color -> Coords -> Square -> [Coords]
  possibleDestinationsFromOrigin = possibleDestinationsFromOriginNoMover movePieceAndRemoveCheck

  coordsToMoves :: Coords -> [Coords] -> [Move]
  coordsToMoves origin = L.map (\dest -> (origin, dest))

  possibleMovesFromOrigin :: Board -> Color -> Coords -> Square -> [Move]
  possibleMovesFromOrigin board color origin square = coordsToMoves origin $ possibleDestinationsFromOrigin board color origin square

  statePossibleMoves :: GameState -> [Move]
  statePossibleMoves (board, color) = reduceBoard (possibleMovesFromOrigin board color) (\ result moves -> result L.++ moves) [] board

  movePawn :: Board -> Color -> Coords -> [Coords]
  movePawn board color origin = (movePawnStraight board color origin) L.++ (movePawnEat board color origin)

  movePawnStraight :: Board -> Color -> Coords -> [Coords]
  movePawnStraight board color (x, y) =
    L.filter
      (\coords -> isInBoard coords && (getSquare board coords) == Empty)
      ([(x, y+pawnDirectionFactor)] L.++ (if y==1 && color == Black || y == 6 && color == White then [(x, y+2*pawnDirectionFactor)] else []))
    where
      pawnDirectionFactor = pawnDirection color

  -- TODO3 - Write en passant rule
  movePawnEat :: Board -> Color -> Coords -> [Coords]
  movePawnEat board color (x, y) = (
    L.filter (
      \coords -> isInBoard coords && isPlayerPiece (next color) (getSquare board coords)
    ) [(x+1, y+pawnDirectionFactor), (x-1, y+pawnDirectionFactor)])
    where
      pawnDirectionFactor = pawnDirection color

  moveInAllDirections :: Int -> Board -> Color -> Coords -> [Coords]
  moveInAllDirections distance board coords origin = (moveInLine distance board coords origin) L.++ (moveInDiagonal distance board coords origin)

  moveInLine :: Int -> Board -> Color -> Coords -> [Coords]
  moveInLine distance board color (x, y) = L.concatMap (checkDirection distance board color) [(\d -> (x+d, y)), (\d -> (x-d, y)), (\d -> (x, y+d)), (\d -> (x, y-d))]

  moveInDiagonal :: Int -> Board -> Color -> Coords -> [Coords]
  moveInDiagonal distance board color (x, y) = L.concatMap (checkDirection distance board color) [(\ d -> (x+d, y+d)), (\ d -> (x+d, y-d)), (\ d -> (x-d, y-d)), (\ d -> (x-d, y+d))]

  moveHorse :: Board -> Color -> Coords -> [Coords]
  moveHorse board color (x, y) =  L.filter (\coords ->
    let square = getSquare board coords in
    (isInBoard coords) && (not (isPlayerPiece color square))
    ) [(x+2, y+1), (x+1, y+2), (x+2, y-1), (x+1, y-2), (x-2, y-1), (x-1, y-2), (x-2, y+1), (x-1, y+2)]

  checkDirection :: Int -> Board -> Color -> (Int -> Coords) -> [Coords]
  checkDirection distance board color transformator = L.foldr (\coords ys ->
    case canContinueInDirection board color coords of
      Nothing -> []
      Just False -> [coords]
      Just True -> (coords:ys)
    ) [] $ L.map transformator [1..distance]

  canContinueInDirection :: Board -> Color -> Coords -> Maybe Bool
  canContinueInDirection board color coords
    | (not . isInBoard $ coords) || (isPlayerPiece color square) = Nothing -- Stop scanning
    | otherwise = Just (square == Empty) -- Empty: keep scanning, opponent piece: stop scanning but possible move
    where
      square = getSquare board coords

  pawnDirection :: Color -> Int
  pawnDirection White = -1
  pawnDirection Black = 1

  removeCheckPosition :: Board -> Color -> Coords  -> [Coords] -> [Coords]
  removeCheckPosition board color origin = L.filter (\destination -> isNothing $ checkingPiece (boardMove board (origin, destination)) color)

  checkingPiece :: Board -> Color -> Maybe Coords
  checkingPiece board color = findBoard (\origin square ->
      kingCoords `L.elem` (possibleDestinationsFromOriginNoMover basicPieceTypeMover board (next color) origin square)
    ) board
    where
      kingCoords = fromJust (findBoard (\coords square -> (isKing square) && (getColor square == Just color)) board)

