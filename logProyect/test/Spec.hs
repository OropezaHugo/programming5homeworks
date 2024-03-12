import Log
import Test.QuickCheck


main :: IO [LogMessage]
main = testParse parseFileOfMessages 50 "src/sample.log"