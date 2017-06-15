module Board where
  import Data.Vector as V
  import Data.List as L
  import Datatypes
  import Data.Char
  import Data.Maybe

  next :: Color -> Color
  next White = Black
  next Black = White

  writeBoard :: Board -> String
  writeBoard board = (intercalate "\n" $ toList $ V.map (toList . V.map (L.head . show)) board) L.++ "\n"

  boardLineToAscii :: Int -> Vector Square -> String
  boardLineToAscii nb line = (show nb) L.++ " ║" L.++ (unwords (L.map squareToAscii (toList line))) L.++ "║ "  L.++ (show nb)

  boardToAscii :: Board -> String
  boardToAscii board = unlines ([
    "   A B C D E F G H   ",
    "  ╔═══════════════╗  "] L.++
    (toList (V.imap (\lineNb line -> boardLineToAscii (8 - lineNb) line) board)) L.++
    ["  ╚═══════════════╝  ",
    "   A B C D E F G H   "
    ])

  matrixToLists :: Vector (Vector a) -> [[a]]
  matrixToLists = toList . V.map toList

  reduceLine :: (a -> Int -> Square -> a) -> a -> Vector Square -> a
  reduceLine fn accumulator line = V.ifoldl fn accumulator line

  imapBoard :: (Coords -> Square -> a) -> Board -> Vector (Vector a)
  imapBoard fn board = imap (\lineNb line -> imap (\colNb square -> fn (colNb, lineNb) square) line) board

  reduceBoard :: (Coords -> Square -> a) -> (b -> a -> b) -> b -> Board -> b
  reduceBoard mapFn reduceFn accumulator board =
    let mappedBoard = imapBoard mapFn board in
      V.foldl (\acc line -> V.foldl reduceFn acc line) accumulator mappedBoard

  findBoard :: (Coords -> Square -> Bool) -> Board -> Maybe Coords
  findBoard findFn board = reduceBoard (\origin square -> (origin, square)) (\result (origin, square) -> case () of
    _ | isJust result -> result
      | findFn origin square -> Just origin
      | otherwise -> Nothing
    ) Nothing board

  -- TODO5: make getSquare secure (with a Maybe?)
  getSquare :: Board -> Coords -> Square
  getSquare board (x, y) = board!y!x

  isKing :: Square -> Bool
  isKing (Piece (pieceType, _)) = pieceType == King
  isKing Empty = False

  getColor :: Square -> Maybe Color
  getColor (Piece (_, color)) = Just color
  getColor Empty = Nothing

  gameOverWinner :: Board -> Maybe Color
  gameOverWinner board =
    let kings = V.concatMap (V.filter isKing) board
    in if V.length kings == 2 then Nothing else getColor (V.head kings)

  readBoard :: String -> Board
  readBoard boardString = fromList $ L.map fromList (L.map (L.map (read . (:[]))) $ lines boardString)

  makeMove :: GameState -> Move -> GameState
  makeMove (board, color) move = (boardMove board move, next color)

  boardMove :: Board -> Move -> Board
  boardMove board move = imapBoard (replaceSquare board move) board

  movePieceAndPromoteIfNecessary :: Square -> Coords -> Square
  movePieceAndPromoteIfNecessary (Piece (Pawn, color)) (x, y) = Piece (if (color == White && y == 0 || color == Black && y == 7) then Queen else Pawn, color)
  movePieceAndPromoteIfNecessary square _ = square

  replaceSquare ::  Board -> Move -> Coords -> Square -> Square
  replaceSquare board (origin, destination) boardCoords destinationSquare
    | boardCoords == origin = Empty
    | boardCoords == destination = originSquare
    -- | boardCoords == destination = movePieceAndPromoteIfNecessary originSquare origin
    | otherwise = destinationSquare
    where
      originSquare = getSquare board origin
