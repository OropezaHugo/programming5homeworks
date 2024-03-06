module Testrevision where
    split :: String -> Char -> [String]
    split "" _ = []

    listOfStudents :: String -> [(String, Int)]
    listOfStudents "" = ()
    listOfStudents s = map (\(x:y:[]) -> (x, read y)) map (\x -> split x " ") (split s ",")

    averageAge :: String -> Int 
    averageAge s = div (sum map (\(x,y) -> y) (listOfStudents s)) length (listOfStudents s)

    studentYoungerThan :: String -> Int -> [(String, Int)]
    studentYoungerThan s a = filter(\(x, y) -> y < a) (listOfStudents s)

    studentsInBetween :: String -> Int -> Int -> [(String, Int)]
    studentYoungerThan s l g = filter(\(x, y) -> y > l && y < g ) (listOfStudents s)


    
