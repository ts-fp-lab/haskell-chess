module Main where

  import Data.Char
  import Data.List
  import Data.List.Utils
  import Data.List.Split
  import Data.Foldable
  import Data.Time.Clock
  import System.IO
  import System.Directory

  import Datatypes
  import Board
  import Rules

  -- type Choice = (String, String)

  -- data Choice a = (String, String, a)


  chessCoords :: Coords -> String
  chessCoords (x, y) = [Data.Char.chr (x+65)] ++ (show (8 - y))

  chessMove :: Move -> String
  chessMove (origin, destination) = (chessCoords origin) ++ "->" ++ (chessCoords destination)

  -- indexesToCoords :: Int -> Int -> Coords
  -- indexesToCoords x y = (read $ (:[]) $ Data.Char.chr ((Data.Char.ord (head (show (minBound :: Letter))) + x)), 8 - y)

  stringToCoords :: Char -> Char -> Coords
  stringToCoords letter number = (x, y)
    where
      x = ((Data.Char.ord $ toUpper letter) - 65) `mod` 8
      y = (8 - read [number]) `mod` 8

  stringToMove :: String -> Maybe Move
  stringToMove s = do
      l1:n1:l2:n2:_ <- return s
      return (stringToCoords l1 n1, stringToCoords l2 n2)
  -- writeBoard :: Board -> String
  -- writeBoard board = (intercalate "\n" $ toList $ Data.Vector.map (toList . Data.Vector.map (head . show)) board) ++ "\n"

  moveOrCommandIO :: String -> Either (Maybe Move) [String]
  moveOrCommandIO (':':rest) = Right (splitOn " " rest)
  moveOrCommandIO string = Left (stringToMove string)

  --------
  -- IO --
  --------

  queryMove :: GameState -> IO (Either Move [String])
  queryMove gameState = do
    moveIO <- getLine
    case moveOrCommandIO moveIO of
      Left Nothing -> do
        putStrLn "Wrong Input. Retry!"
        queryMove gameState
      Left (Just move) -> do
        if isMoveAllowed gameState move then
          return (Left move)
        else do
          putStrLn "Impossible Move. Retry!"
          putStrLn ("Possible Moves:" ++ concatMap chessCoords (possibleMoves gameState (fst move)))
          queryMove gameState
      Right command -> return (Right command)


  gameTurn :: GameState -> String -> IO ()
  gameTurn (board, color) gameName = do
    putStr $ boardToAscii board
    putStr ((show color) ++ "'s turn. Move? (eg. d2d4)\n")
    moveIO <- queryMove (board, color)
    case moveIO of
      Left move -> do
        let newBoard = makeMove board move
        let winner = gameOverWinner newBoard
        case winner of
          Nothing -> do
            saveMoveToFile move gameName
            gameTurn (newBoard, next color) gameName
          Just winner -> putStr ((show winner) ++ " won!")
      Right command -> do
        exit <- executeCommandExits (board, color) gameName command
        if not exit
          then gameTurn (board, color) gameName
          else quitGame gameName

  executeCommandExits :: GameState -> String -> [String] -> IO Bool
  executeCommandExits state _ ("quit":rest) = do
    return True
  executeCommandExits state oldGameName ("save":newGameName:rest) = do
    saveGame oldGameName newGameName
    return False
  executeCommandExits _ _ _ = do
    putStrLn ("Unknown command")
    return False

  -- TODO: Remove the first of the triplet and make it deducable from first char of second and indice
  choiceMaker :: String -> [(String, String, a)] -> IO a
  choiceMaker question choices = do
    putStrLn question
    forM_ choices (\(_, str, _) -> putStrLn str)
    choiceIO <- getLine
    case find (\(match, _, _) -> toUpper (head choiceIO) `elem` match) choices of
      Nothing -> do
        putStrLn "Wrong Choice. Try again..."
        choiceMaker question choices
      Just (_, _, result) -> return result

  choosePlayer :: IO Player
  choosePlayer = choiceMaker "Who will you beat?" [
      ("1H", "1. Human (h)", Human), -- ("Human", Human)
      ("2I", "2. IA (i)", IA) -- ("IA", IA)
    ]

  chooseColor :: IO Color
  chooseColor = choiceMaker "What player do you want to be?" [
      ("1W", "1. White (w)", White),
      ("2B", "2. Black (b)", Black)
    ]

  getInitialGameState :: IO GameState
  getInitialGameState = do
    boardIO <- readFile "data/board.txt"
    let board = readBoard boardIO
    return (board, White)

  buildFileName :: String -> Bool -> String
  buildFileName gameName temp = "games/" ++ (if temp then "." else "") ++ gameName ++ ".chess"

  newGame :: IO ()
  newGame = do
    putStrLn "Starting new game"
    player <- choosePlayer
    initialGameState <- getInitialGameState
    currentTime <- getCurrentTime
    gameTurn initialGameState (show currentTime)

  loadGame :: IO ()
  loadGame = do
    putStrLn "Here are the available games:"
    availableGames <- getDirectoryContents "games"
    forM_ (map ((replace ".chess" "") . ("- " ++)) (filter (\x -> (not (startswith "." x)) && (endswith ".chess" x)) availableGames)) putStrLn
    -- TODO: make this a list with index to be able to type a number
    putStrLn "Type the name you want to load:"
    gameName <- getLine
    let loadFileName = (buildFileName gameName False)
    let tempFileName = (buildFileName gameName True)
    copyFile loadFileName tempFileName
    fileContents <- readFile tempFileName
    -- let board = readBoard fileContents
    let moves = map (read::String->Move) $ filter ("" /=) $ splitOn "\n" fileContents
    initialGameState <- getInitialGameState
    let newGameState = foldl (\(board, color) move -> (makeMove board move, next color)) initialGameState moves
    gameTurn newGameState gameName

  saveGame :: String -> String -> IO ()
  saveGame oldGameName newGameName = do
    copyFile (buildFileName oldGameName True) (buildFileName newGameName False) 
    putStrLn "Game saved"

  quitGame :: String -> IO ()
  quitGame oldGameName = do
    removeFile (buildFileName oldGameName True)
    putStrLn "Bye"

  saveMoveToFile :: Move -> String -> IO ()
  saveMoveToFile move gameName = do
    appendFile (buildFileName gameName True) ((show move) ++ "\n")

  startOrLoad :: IO ()
  startOrLoad = do
    loadSavedGame <- choiceMaker "What do you want to do?" [
        ("1S", "  1. Start a new game (s)", False),
        ("2L", "  2. Load a game (l)", True)
      ]
    if loadSavedGame then
      loadGame
    else
      newGame

  main :: IO ()
  main = do
    putStrLn "----------------------------"
    putStrLn "| Welcome to Haskell Chess |"
    putStrLn "----------------------------"
    putStrLn ""
    startOrLoad


