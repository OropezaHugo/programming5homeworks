module Lib( someFunc) where

import Log
import MessageTreeFunctions
someFunc :: IO [Maybe LogMessage]
--someFunc = print (testParse parseFileOfMessages 50 "../sample.log" )
someFunc =  testParse parseFileOfMessages 50 "src/sample.log"
