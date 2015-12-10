import String
import Graphics.Element exposing (Element)

import Spreadsheet exposing (..)
import SpreadsheetTypes exposing (..)
import Parse exposing (..)
import ElmTest exposing (..)

testParse string result =
  test ("Parsing string "++string) (assertEqual result (parse string) )
testParseNumber: String -> Float -> Test
testParseNumber string float =
  testParse string (Ok (Value (Number float)))

tests : Test
tests =
    suite "Parsing numbers"
          (List.map (uncurry testParseNumber)
              [("42", 42)
              ,("42.0", 42)
              ,("=42", 42)
              ,("0", 0)

              ])


main : Element
main =
    elementRunner tests
