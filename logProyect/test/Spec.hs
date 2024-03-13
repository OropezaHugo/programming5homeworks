import Log
import Test.QuickCheck


main :: IO [Maybe LogMessage]
main = testParse parseFileOfMessages 50 "src/sample.log"