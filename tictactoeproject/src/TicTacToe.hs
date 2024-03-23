module TicTacToe(startTicTacToe) where
    import System.Console.ANSI
    import Data.List

    type Board = [[Char]]


    emptyBoard :: Board
    emptyBoard = [
        [' ', ' ', ' '],
        [' ', ' ', ' '],
        [' ', ' ', ' ']]

    playTicTacToe :: Board -> Integer -> IO()
    playTicTacToe [] _ = putStrLn "ingrese tablero valido"
    playTicTacToe board n   | n == 0 = putStrLn "empate"
                            | reviewWin board && n `mod` 2 == 0 = putStrLn "X gana, que pro"
                            | reviewWin board = putStrLn "X fué humillado por O"
                            | n `mod` 2 == 0 = putStrLn "Ingrese jugada(O):" 
                            >> getLine >>= \str -> printBoard (reloadBoard str 'O' board) >>
                            if (reloadBoard str 'O' board) /= board then playTicTacToe (reloadBoard str 'O' board) (n - 1)
                            else playTicTacToe board n
                            | otherwise = putStrLn "Ingrese jugada(X):" 
                            >> getLine >>= \str -> printBoard (reloadBoard str 'X' board) >> 
                            if (reloadBoard str 'X' board) /= board then playTicTacToe (reloadBoard str 'X' board) (n - 1)
                            else playTicTacToe board n

    startTicTacToe :: IO()
    startTicTacToe = playTicTacToe emptyBoard 9


    reloadBoard :: String -> Char -> Board -> Board
    reloadBoard str c board = reloadBoardPlay (stringToCordinates str) c board

    reloadBoardPlay :: (Integer, Integer) -> Char -> Board -> Board
    reloadBoardPlay (x, y) c board = updateBoard c x y board

    printRow :: [Char] -> IO ()
    printRow row =  putStrLn ("|" ++ intersperse '|' row ++ "|")

    printBoard :: Board -> IO ()
    printBoard [] = putStrLn "--------"
    printBoard (x:xs) = putStrLn "--------" >> printRow x >> printBoard xs

    updateBoard :: Char -> Integer -> Integer -> Board -> Board
    updateBoard c _ _ [] = [[c]]
    updateBoard c 0 col (x:xs) = replaceAt x col c : xs
    updateBoard c row col (x:xs)    | row < 0 || row >= 3 || col < 0 || col >= 3 = x:xs
                                    | otherwise = x : updateBoard c (row - 1) col xs

    replaceAt :: [Char] -> Integer -> Char -> [Char]
    replaceAt [] _ a = [a]
    replaceAt (x:xs) 0 a    | x == ' ' = a:xs
                            | otherwise = x:xs
    replaceAt (x:xs) n a
        | n < 0 || n > 3 = error "index out of range"
        | otherwise = x : replaceAt xs (n - 1) a




    reviewWin :: Board -> Bool
    reviewWin [] = False
    reviewWin board     = reviewRows board
                        || reviewCols board
                        || reviewDiags board


    reviewRows :: Board -> Bool
    reviewRows ([x,y,z]:xs)     | x /= ' ' && x == y && y == z = True
                                | otherwise = reviewRows xs
    reviewRows board = False

    reviewCols :: Board -> Bool
    reviewCols [x, y, z]
        = head x /= ' ' && head x == head y && head y == head z
        || head (tail x) /= ' ' && head (tail x) == head (tail y) && head (tail y) == head (tail z)
        || head (tail (tail x)) /= ' ' && head (tail (tail x)) == head (tail (tail y)) && head (tail (tail y)) == head (tail (tail z))
    reviewCols board = False

    reviewDiags :: Board -> Bool
    reviewDiags [x, y, z]   = head x /= ' ' && head x == head (tail y) && head (tail y) == head (tail (tail z))
                            || head z /= ' ' && head z == head (tail y) && head (tail y) == head (tail (tail x))
    reviewDiags board = False

    boolToString :: Bool -> String
    boolToString True = "True"
    boolToString False = "False"

    isNumber :: String -> Bool
    isNumber str    | str == "1" || str == "2" || str == "3" || str == "4" || str == "5" || str == "6" ||
                        str == "7" || str == "8" || str == "9" = True
                    | otherwise = False
    stringToCordinates ::  String -> (Integer, Integer)
    stringToCordinates [] = (4, 4)
    stringToCordinates str  | isNumber (takeWhile (/= ' ') str) && isNumber (tail (dropWhile (/= ' ') str)) = (read (takeWhile (/= ' ') str), read (tail (dropWhile (/= ' ') str))) 
                            | otherwise = (4,4)


        
