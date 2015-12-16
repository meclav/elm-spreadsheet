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

testTokens string result =
  test ("Tokenising "++string++" :") (assertEqual result (tokens string))

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
      ,  suite "Tokens"
          [ testTokens "2+2*3-7" ([NumberToken 2, PlusToken, NumberToken 2, MultToken, NumberToken 3, MinusToken, NumberToken 7])
          , testTokens "sum(2,2.0)" ([FunctionName "SUM", Bra, NumberToken 2, Comma, NumberToken 2.0, Ket])
          , testTokens "sum(a2:bc72,c8)" (
                [FunctionName "SUM", Bra,   ReferenceToken("A", 2),Colon,
                 ReferenceToken("BC",72), Comma, ReferenceToken ("C",8),Ket])
          ]
    ,     test "Bracketing" (assertEqual [(Bra,1),(Bra,2), (NumberToken 5, 2), (Ket, 1), (Ket,0)] (stackifyTokens (tokens "((5))")))
    ]

main : Element
main =
    elementRunner tests
