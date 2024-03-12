module Log
    (MessageType(..),
    LogMessage(..),
    TimeStamp(..), 
    MessageTree(..),
    testParse,
    testWhatWentWrong,
    parseFileOfMessages) 
    where


data MessageType = Info
                | Warning
                | Error Int
                deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
                deriving (Show, Eq)

data MessageTree = Leaf
                | Node MessageTree LogMessage MessageTree
                deriving (Show, Eq)


testParse :: (String -> [LogMessage])
                -> Int
                -> FilePath
                -> IO [LogMessage] 

testParse parse n file = take n . parseFileOfMessages <$> readFile file


testWhatWentWrong :: (String -> [LogMessage])
                -> ([LogMessage] -> [String])
                -> FilePath
                -> IO [String]
testWhatWentWrong parse whatWentWrong file
    = whatWentWrong . parse <$> readFile file


parseFileOfMessages :: String -> [LogMessage]
parseFileOfMessages [] = [Unknown "unknown"]
parseFileOfMessages string 
    = parseMessage (returnUntilChar string '\n') : parseFileOfMessages (tail(dropUntilChar string '\n'))

parseMessage :: String -> LogMessage
parseMessage [] = Unknown "unknown"
parseMessage (x:xs)
    | x == 'I' = parseInfoWarningMessages Info (tail xs)
    | x == 'W' = parseInfoWarningMessages Warning (tail xs)
    | x == 'E' = parseErrorMessages (tail xs)
    | otherwise = Unknown "unknown"

parseInfoWarningMessages :: MessageType -> String -> LogMessage
parseInfoWarningMessages _ [] = Unknown "XD"
parseInfoWarningMessages ty string 
    = LogMessage ty (read (returnUntilChar string ' ')) (tail (dropUntilChar string ' '))

parseErrorMessages :: String -> LogMessage
parseErrorMessages [] = Unknown "XD"
parseErrorMessages string 
    = LogMessage (Error (read (returnUntilChar string ' '))) (read (returnUntilChar (tail (dropUntilChar string ' ')) ' ')) (tail (dropUntilChar (tail (dropUntilChar string ' ')) ' '))

    
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((Unknown _):xs) = whatWentWrong xs
whatWentWrong ((LogMessage Warning timestamp message):xs) = whatWentWrong xs
whatWentWrong ((LogMessage Info timestamp message):xs) = whatWentWrong xs
whatWentWrong (( LogMessage (Error num) timestamp message):xs) 
    | num >= 50 = message : whatWentWrong xs
    | otherwise = whatWentWrong xs


returnUntilChar :: String -> Char-> String
returnUntilChar [] _ = []
returnUntilChar string c = takeWhile (/= c) string


dropUntilChar :: String -> Char-> String
dropUntilChar [] _ = []
dropUntilChar string c = dropWhile (/= c) string
