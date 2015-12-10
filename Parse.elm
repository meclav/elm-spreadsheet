module Parse where

import SpreadsheetTypes exposing (..)
import Regex exposing (..)

type NotMatched = NotMatched

type alias Parser err succ = String -> Result err succ

parse: Parser String CellContent
parse s =
  case find (AtMost 1) (regex "^=(.*)") s |> List.map .submatches of
    match::[]
      -> parseFormula s
    _
      -> case parseExpression s of
          Err _ -> Ok (Value (Text s))
          Ok atom -> Ok (Value atom)




parseFormula: Parser String CellContent
parseFormula s = Ok (Value (Number 1337))


{-
 I am sure you can do this through a port
 Parse and eval arithmetic expressions as JavaScript
 Be as awesome as the angular guy
-}
parseExpression: Parser NotMatched Atom
parseExpression s = Err (NotMatched)




{-
=? Need a bigger formula parser. Else use smaller parser.

Try different parsers and get one you want? Like in the Scala pattern.

What should an intermediate be?
Maybe only tokenise later.
=

-}
