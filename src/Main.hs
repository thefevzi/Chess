import System.Environment (getArgs)
import System.Exit (exitSuccess)
import ChessBoard
import Color
import Position
import Moves
import WinCheck
import Data.Char (ord)
import Data.Maybe (isJust)

parseMove :: String -> Maybe (Position, Position)
parseMove [f1, r1, f2, r2]
    | all (`elem` ['a'..'h']) [f1, f2] && all (`elem` ['1'..'8']) [r1, r2] =
        Just (Position (ord r1 - ord '1') (ord f1 - ord 'a'),
              Position (ord r2 - ord '1') (ord f2 - ord 'a'))
    | otherwise = Nothing
parseMove _ = Nothing

printBoard :: ChessBoard -> IO ()
printBoard = putStrLn . show

-- Game loop for human vs. human
gameLoop :: ChessBoard -> IO ()
gameLoop board = do
    printBoard board
    if isCheckmate board (nextMove board)
    then do
        putStrLn $ "Checkmate! " ++ show (other (nextMove board)) ++ " wins."
        putStrLn "Take your revenge? (y/n):"
        revenge <- getLine
        if revenge == "y" || revenge == "Y"
        then gameLoop initialPosition
        else putStrLn "Maybe Later." >> exitSuccess
    else if isInCheck board (nextMove board)
         then putStrLn $ "Check! " ++ show (nextMove board) ++ " is in check."
         else putStrLn $ "Next move: " ++ show (nextMove board)
    putStrLn "Enter your move (e.g., d2d4):"
    input <- getLine
    case parseMove input of
        Nothing -> do
            putStrLn "Invalid move format. Try again."
            gameLoop board
        Just (from, to) ->
            if isValidMove board from to
            then if isCheckmate (movePiece (switch board) from to) (nextMove board)
                 then do
                     printBoard (movePiece (switch board) from to)
                     putStrLn $ "Checkmate! " ++ show (other (nextMove board)) ++ " wins."
                     putStrLn "Take your revenge? (y/n):"
                     revenge <- getLine
                     if revenge == "y" || revenge == "Y"
                     then gameLoop initialPosition
                     else putStrLn "Maybe Later." >> exitSuccess
                 else gameLoop (movePiece (switch board) from to)
            else do
                putStrLn "Invalid move. Try again."
                gameLoop board

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] -> do
            putStrLn "Starting Human vs. Human Chess Game..."
            putStrLn "Welcome to Haskell Chess"
            gameLoop initialPosition
        ["2"] -> do
            putStrLn "Starting AI vs. AI Chess Game..."
            putStrLn "Welcome to Haskell Chess"
            --exitFailure
            gameLoop initialPosition
