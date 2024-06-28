import Data.Char (ord)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Monad (when)

import Chessboard
import Color
import Position
import Moves
import CheckEnd
import AI

parseMove :: String -> Maybe (Position, Position)
parseMove [f1, r1, f2, r2]
    | all (`elem` ['a'..'h']) [f1, f2] && all (`elem` ['1'..'8']) [r1, r2] =
        Just (Position (ord r1 - ord '1') (ord f1 - ord 'a'),
              Position (ord r2 - ord '1') (ord f2 - ord 'a'))
    | otherwise = Nothing
parseMove _ = Nothing

printBoard :: Chessboard -> IO ()
printBoard = putStrLn . show

ending :: IO ()
ending = do
    response <- getLine
    if response == "y" || response == "Y"
    then gameLoop initialPosition
    else if response == "n" || response == "N"
    then exitSuccess
    else do
        putStrLn "Please enter y/Y or n/N"
        ending

winnerEnding :: Color -> IO ()
winnerEnding winner = do
    putStrLn $ "Checkmate! " ++ show winner ++ " wins."
    putStrLn "Take your revenge? (y/n):"
    ending

stalemateEnding :: IO ()
stalemateEnding = do
    putStrLn "Stalemate! Draw."
    putStrLn "Play again? (y/n):"
    ending

-- Game loop for human vs. human
gameLoop :: Chessboard -> IO ()
gameLoop board = do
    printBoard board
    if isCheckmate board (nextMove board)
    then winnerEnding (other (nextMove board))
    else if isStalemate board (nextMove board)
         then stalemateEnding
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
            case at board from of
                Nothing -> do
                    putStrLn "I can't see any piece in that position"
                    gameLoop board
                Just piece ->
                    if color piece /= nextMove board
                    then do
                        putStrLn "That's not your piece!"
                        gameLoop board
                    else if isValidMove board from to
                        then let newBoard = switch $ movePiece board from to in
                            if isCheckmate newBoard (nextMove newBoard)
                            then do
                                printBoard newBoard
                                winnerEnding (other (nextMove newBoard))
                            else if isStalemate newBoard (nextMove newBoard)
                                 then do
                                    printBoard newBoard
                                    stalemateEnding                                
                                 else gameLoop newBoard
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
            putStrLn "NOTE: Castling is done by moving the king to the position (e.g, e1g1)"
            gameLoop initialPosition
        ["2", depthStr] -> do
            let depth = read depthStr :: Int
            putStrLn "Starting Human vs. AI Chess Game..."
            putStrLn "Welcome to Haskell Chess"
            putStrLn $ "Depth is: " ++ show depth
            gameLoopAI initialPosition depth
        _ -> putStrLn "Usage: chess 1 (for Human vs. Human) or chess 2 <depth> (for Human vs. AI)" >> exitSuccess
