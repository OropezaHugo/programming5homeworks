module Log
    (MessageType(..),
    LogMessage(..),
    TimeStamp(..), 
    MessageTree(..),
    testParse,
    testWhatWentWrong,
    parseFileOfMessages,
    whatWentWrong) 
    where


data MessageType = Info
                | Warning
                | Error (Maybe Int)
                deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage (Maybe MessageType) (Maybe TimeStamp) (Maybe String)
                | Unknown (Maybe String)
                deriving (Show, Eq)

data MessageTree = Leaf
                | Node (Maybe MessageTree) (Maybe LogMessage) (Maybe MessageTree)
                deriving (Show, Eq)


testParse :: (String -> [Maybe LogMessage])
                -> Int -> FilePath -> IO [Maybe LogMessage] 

testParse parseFileOfMessages n file = take n . parseFileOfMessages <$> readFile file

testWhatWentWrong :: (String -> [Maybe LogMessage])
                -> ([Maybe LogMessage] -> [String]) -> FilePath -> IO [String]
testWhatWentWrong parseFileOfMessages whatWentWrong file = whatWentWrong . parseFileOfMessages <$> readFile file


parseFileOfMessages :: String -> [Maybe LogMessage]
parseFileOfMessages [] = [Just (Unknown (Just "unknown"))]
parseFileOfMessages string
    = parseMessage (returnUntilChar string '\n') : parseFileOfMessages (tail(dropUntilChar string '\n'))

parseMessage :: String -> Maybe LogMessage
parseMessage [] = Just (Unknown (Just "unknown"))
parseMessage (x:xs)
    | x == 'I' = parseInfoWarningMessages (Just Info) (Just(tail xs))
    | x == 'W' = parseInfoWarningMessages (Just Warning) (Just(tail xs))
    | x == 'E' = parseErrorMessages (Just(tail xs))
    | otherwise = Just (Unknown (Just "unknown"))

parseInfoWarningMessages :: Maybe MessageType -> Maybe String -> Maybe LogMessage
parseInfoWarningMessages _ Nothing = Just (Unknown (Just "XD"))
parseInfoWarningMessages _ (Just []) = Just (Unknown (Just "XD"))
parseInfoWarningMessages Nothing _ = Just (Unknown (Just "XD"))
parseInfoWarningMessages (Just ty) (Just string )
    = Just (LogMessage (Just ty) (Just (read (returnUntilChar string ' '))) (Just (tail (dropUntilChar string ' '))))

parseErrorMessages :: Maybe String -> Maybe LogMessage
parseErrorMessages Nothing = Just (Unknown (Just "XD"))
parseErrorMessages (Just []) = Just (Unknown (Just "XD"))
parseErrorMessages (Just string) 
    = Just (LogMessage (Just (Error (read (returnUntilChar string ' ')))) (read (returnUntilChar (tail (dropUntilChar string ' ')) ' ')) (Just (tail (dropUntilChar (tail (dropUntilChar string ' ')) ' '))))

    
whatWentWrong :: [Maybe LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong ((Just (Unknown _)):xs) = whatWentWrong xs
whatWentWrong ((Just (LogMessage (Just Warning) timestamp message)):xs) = whatWentWrong xs
whatWentWrong ((Just (LogMessage (Just Info) timestamp message)):xs) = whatWentWrong xs
whatWentWrong ((Just (LogMessage (Just (Error num)) timestamp (Just message))):xs) 
    | num >= (Just 50) = message : whatWentWrong xs
    | otherwise = whatWentWrong xs


returnUntilChar :: String -> Char-> String
returnUntilChar [] _ = []
returnUntilChar string c = takeWhile (/= c) string


dropUntilChar :: String -> Char-> String
dropUntilChar [] _ = []
dropUntilChar string c = dropWhile (/= c) string
