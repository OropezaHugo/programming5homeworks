module Lib( someFunc) where

import Log
import MessageTreeFunctions

someFunc :: IO [String]
--someFunc :: IO [Maybe LogMessage]
someFunc = testWhatWentWrong parseFileOfMessages whatWentWrong "src/sample.log"
--someFunc =  testParse parseFileOfMessages 50 "src/sample.log"
