module Board where
  import Data.Vector as V
  import Data.List as L
  import Datatypes
  import Data.Char

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

  -- concat :: [[a]] -> [a]
  -- concat xss = L.foldr (L.++) [] xss

  matrixToLists :: Vector (Vector a) -> [[a]]
  matrixToLists = toList . V.map toList

  getSquare :: Board -> Coords -> Square
  getSquare board (x, y) = board!y!x

  isKing :: Square -> Bool
  isKing (Piece (piecetype, _)) = piecetype == King
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

  makeMove :: Board -> Move -> Board
  makeMove board move = imap (\y line ->
    imap (\x square -> replaceSquare board move square (x, y)) line) board

  replaceSquare ::  Board -> Move -> Square -> Coords -> Square
  replaceSquare board (origin, destination) square coords
    | coords == origin = Empty
    | coords == destination = getSquare board origin
    | otherwise = square