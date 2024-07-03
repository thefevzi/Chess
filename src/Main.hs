import Data.Char (ord)
import Data.Maybe (isJust)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

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

ending :: (Chessboard -> IO ()) -> IO ()
ending gameLoopFn = do
    putStrLn "Do you want to play again? (y/n):"
    response <- getLine
    if response == "y" || response == "Y"
    then gameLoopFn initialPosition
    else if response == "n" || response == "N"
    then exitSuccess
    else do
        putStrLn "Please enter y/Y or n/N"
        ending gameLoopFn

winnerEnding :: Color -> (Chessboard -> IO ()) -> IO ()
winnerEnding winner gameLoopFn = do
    putStrLn $ "Checkmate! " ++ show winner ++ " wins."
    ending gameLoopFn

stalemateEnding :: (Chessboard -> IO ()) -> IO ()
stalemateEnding gameLoopFn = do
    putStrLn "Stalemate! Draw."
    ending gameLoopFn

colorAI :: IO Color
colorAI = do
    putStrLn "Which color do you want the AI to play as? (w/b):"
    aiColorInput <- getLine
    return $ if aiColorInput == "w" || aiColorInput == "W" then White else Black

startingState :: Chessboard -> (Chessboard -> IO ()) -> IO ()
startingState board gameLoopFn = do
    printBoard board
    if isCheckmate board (nextMove board)
    then winnerEnding (other (nextMove board)) gameLoopFn
    else if isStalemate board (nextMove board)
         then stalemateEnding gameLoopFn
         else if isInCheck board (nextMove board)
              then putStrLn $ "Check! " ++ show (nextMove board) ++ " is in check."
              else putStrLn $ "Next move: " ++ show (nextMove board)

movingState :: Chessboard -> (Chessboard -> IO ()) -> IO ()
movingState board gameLoopFn = do
    putStrLn "Enter your move (e.g., d2d4):"
    input <- getLine
    case parseMove input of
        Nothing -> do
            putStrLn "Invalid move format. Try again."
            gameLoopFn board
        Just (from, to) ->
            case at board from of
                Nothing -> do
                    putStrLn "I can't see any piece in that position"
                    gameLoopFn board
                Just piece ->
                    if color piece /= nextMove board
                    then do
                        putStrLn "That's not your piece!"
                        gameLoopFn board
                    else if isValidMove board from to
                        then let newBoard = switch $ movePiece board from to in
                            if isCheckmate newBoard (nextMove newBoard)
                            then do
                                printBoard newBoard
                                winnerEnding (other (nextMove newBoard)) gameLoopFn
                            else if isStalemate newBoard (nextMove newBoard)
                                 then do
                                    printBoard newBoard
                                    stalemateEnding gameLoopFn
                                 else gameLoopFn newBoard
                    else do
                        putStrLn "Invalid move. Try again."
                        gameLoopFn board


-- Game loop for human vs. human --
gameLoop :: Chessboard -> IO ()
gameLoop board = do

    startingState board gameLoop

    movingState board gameLoop


-- Game loop for human vs. AI --
gameLoopAI :: Chessboard -> Color -> Int -> IO ()
gameLoopAI board aiColor depth = do

    startingState board (\b -> gameLoopAI b aiColor depth)

    if nextMove board == aiColor
    then do
        let (from, to) = bestMove board aiColor depth
        let newBoard = switch $ movePiece board from to
        if isCheckmate newBoard (nextMove newBoard)
        then do
            printBoard newBoard
            winnerEnding (other (nextMove newBoard)) (\b -> gameLoopAI b aiColor depth)
        else if isStalemate newBoard (nextMove newBoard)
             then do
                printBoard newBoard
                stalemateEnding (\b -> gameLoopAI b aiColor depth)
             else gameLoopAI newBoard aiColor depth
    else movingState board (\b -> gameLoopAI b aiColor depth)

main :: IO ()
main = do
    args <- getArgs
    case args of

        ["1"] -> do
            putStrLn "Starting Human vs. Human Chess Game...\nWelcome to Haskell Chess"
            putStrLn "NOTE: Castling is done by moving the king to the position (e.g, e1g1)"
            gameLoop initialPosition

        ["2", depthStr] -> do
            let depth = read depthStr :: Int
            putStrLn "Starting Human vs. AI Chess Game...\nWelcome to Haskell Chess"
            putStrLn "NOTE: Castling is done by moving the king to the position (e.g, e1g1)"
            putStrLn $ "Depth is: " ++ show depth
            aiColor <- colorAI
            gameLoopAI initialPosition aiColor depth

        _ -> putStrLn "Usage: chess 1 (for Human vs. Human) or chess 2 <depth> (for Human vs. AI)" >> exitSuccess
