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

testFloatTokenizer s tokExpected =
  defaultTest (assertEqual (tokExpected::[]) (snd <| floatTok s) )

tests : Test
tests =
    suite "Tests" [
      suite "Parsing numbers"
          (List.map (uncurry testParseNumber)
              [("42", 42)
              ,("42.0", 42)
              ,("=1337", 1337)
              ,("0", 0)

              ])
      , testParse "foo" (Ok (Value (Text "foo")))
      , suite "Float tokenizer"
          [ testFloatTokenizer "123" (NumberToken 123)
          , testFloatTokenizer "1.3" (NumberToken 1.3)
          , testFloatTokenizer "   0 " (NumberToken 0)
          ]
    ]

main : Element
main =
    elementRunner tests
