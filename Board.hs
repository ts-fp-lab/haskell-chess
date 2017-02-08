module Board where
  import Data.Vector
  import Datatypes
  import Data.Char
  import Data.List

  writeBoard :: Board -> String
  writeBoard board = (intercalate "\n" $ toList $ Data.Vector.map (toList . Data.Vector.map (Data.List.head . show)) board) Data.List.++ "\n"

  boardLineToAscii :: Int -> Vector Square -> String
  boardLineToAscii nb line = (show nb) Data.List.++ " ║" Data.List.++ (unwords (Data.List.map squareToAscii (toList line))) Data.List.++ "║ "  Data.List.++ (show nb)

  boardToAscii :: Board -> String
  boardToAscii board = unlines ([
    "   A B C D E F G H   ",
    "  ╔═══════════════╗  "] Data.List.++
    (toList (Data.Vector.imap (\lineNb line -> boardLineToAscii (8 - lineNb) line) board)) Data.List.++
    ["  ╚═══════════════╝  ",
    "   A B C D E F G H   "
    ])


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
    let kings = Data.Vector.concatMap (Data.Vector.filter isKing) board
    in if Data.Vector.length kings == 2 then Nothing else getColor (Data.Vector.head kings)

  readBoard :: String -> Board
  readBoard boardString = fromList $ Data.List.map fromList (Data.List.map (Data.List.map (read . (:[]))) $ lines boardString)

  makeMove :: Board -> Move -> Board
  makeMove board move = imap (\y line ->
    imap (\x square -> replaceSquare board move square (x, y)) line) board

  replaceSquare ::  Board -> Move -> Square -> Coords -> Square
  replaceSquare board (origin, destination) square coords
    | coords == origin = Empty
    | coords == destination = getSquare board origin
    | otherwise = square