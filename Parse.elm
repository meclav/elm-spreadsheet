module Parse where

import SpreadsheetTypes exposing (..)
import Regex exposing (..)
import String exposing (toFloat)

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

type Token = Unknown String | FunctionName String | Bra | Ket | Plus | Minus | NumberToken Float | TextToken String --...
type alias Tokenizer = String -> (String, List Token)

tokens: String -> List Token
tokens s =
  let
    tokensInternal s acc = case s of
       ""
        -> acc
       _
        ->
          let
              tokenizers : List Tokenizer
              tokenizers =
                [tok "(" Bra
                ,tok ")" Ket
                ,tok "+" Plus
                ,tok "-" Minus
                ,floatTok

                      ]
              firstThatWorks remainingTokenizers s = case remainingTokenizers of
                 []
                  -> ("", [Unknown s]) --can't consume!
                 t::ts
                  -> case t s of
                      (rem,[]) -> firstThatWorks ts s
                      (rem, tokens) -> (rem,tokens)
          in case firstThatWorks tokenizers s of
            (rem, tokens)
              -> tokensInternal rem (tokens++acc)
  in tokensInternal s []

floatTok = ctok "(\\d+(?:\\.\\d+)?)" (\n-> case String.toFloat n of 
      Err e
        -> TextToken n
      Ok f
        -> NumberToken f
        )

tok literalString out s =
  simpleTok (ourRegex (Regex.escape literalString)) (out::[]) s

rtok regexString out s =
  simpleTok (ourRegex regexString) (out::[]) s

ctok regexWithCaptureGroup out s =
  captureTok (ourRegex regexWithCaptureGroup) (\s-> (out s)::[]) s


ourRegex regexString = Regex.regex ("^(?: *)"++regexString++"(.*)")

-- regex of the form "^regex(.*)"
simpleTok regex outTokens s =
                case find (AtMost 1) regex  s of
                    match::[]
                      -> case match.submatches of
                          Just theUnparsedBit :: []
                            -> (theUnparsedBit, outTokens)
                          _
                            -> (s,[])
                    _
                      -> (s, [])

-- regex of the form "^regexWithOneCaptureGroup(.*)"
captureTok regex makeTokFromCaptureGroup s =
                case find (AtMost 1) regex  s of
                    match::[]
                      -> case match.submatches of
                          Just captureGroup :: Just theUnparsedBit :: []
                            -> (theUnparsedBit, makeTokFromCaptureGroup captureGroup)
                          _
                            -> (s,[])
                    _
                      -> (s, [])
