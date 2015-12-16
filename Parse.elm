module Parse where

import SpreadsheetTypes exposing (..)
import Regex exposing (..)
import String exposing (toFloat,toInt,toUpper)

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



parseFormula s = (Ok (Value (Number 1337)))
{-
 I am sure you can do this through a port
 Parse and eval arithmetic expressions as JavaScript
 Be as awesome as the angular guy
-}
parseExpression: Parser NotMatched Atom
parseExpression s = Err (NotMatched)

--todo: range union and intersect, cell range according to http://www.nsl.com/k/excel.k
type Token = Unknown String | FunctionName String | Bra | Ket | Comma
            | PlusToken | MinusToken | MultToken | DivToken |PowToken
            | NumberToken Float | TextToken String
            | ReferenceToken Ref | Colon
            | Quote
            | Eq | Le | Gr| Neq| Leq| Greq

type alias Ref = (String, Int)
type alias Tokenizer = String -> (String, List Token)

stackifyTokens: List Token -> List (Token, Int)
stackifyTokens ts =
  let stackifyTokensInternal ts n = case ts of
    []
      -> []
    x::xs
      -> let d = case x of
          Bra -> 1
          Ket -> -1
          _   -> 0
         in (x,n+d) :: (stackifyTokensInternal xs (n+d))
  in stackifyTokensInternal ts 0

{-
type Atom = Number Float | Text String | Error
type AST a op = Node op (List (AST a op)) | Leaf a
type CellContent = Value Atom | Reference GridId | Formula (AST CellContent Op)
-}
type TokenTree = AST (List Token) ()


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
                ,tok "," Comma
                ,tok "+" PlusToken
                ,tok "-" MinusToken
                ,tok "*" MultToken
                ,tok "/" DivToken
                ,tok "^" PowToken
                ,floatTok
                ,ctok "(\\w+[\\w\\d\\.]*)\\(" (\s -> FunctionName (toUpper s) :: Bra::[])
                ,referenceTok
                ,tok ":" Colon
                ,tok "\"" Quote --"=";"<";">";"<>";"<=";">="
                ,tok "=" Eq
                ,tok "<" Le
                ,tok ">" Gr
                ,tok "<>" Neq
                ,tok "<=" Leq
                ,tok ">=" Greq
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
              -> tokensInternal rem (acc++tokens)
  in tokensInternal s []

floatTok = ctok "(\\d+(?:\\.\\d+)?)" (\n-> (case String.toFloat n of
      Err e
        -> TextToken n
      Ok f
        -> NumberToken f
        )::[])

tok literalString out s =
  simpleTok (ourRegex (Regex.escape literalString)) (out::[]) s

rtok regexString out s =
  simpleTok (ourRegex regexString) (out::[]) s

ctok regexWithCaptureGroup out s =
  captureTok (ourRegex regexWithCaptureGroup) (\s-> (out s)) s


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

referenceTok s = case find (AtMost 1) (ourRegex ("([a-zA-Z]+)(\\d+)"))  s of
                    match::[]
                      -> case match.submatches of
                          Just rows::Just cols :: Just theUnparsedBit :: []
                            -> case String.toInt cols of
                                  Err e
                                    -> (s,[])
                                  Ok f
                                    -> (theUnparsedBit, [ReferenceToken (toUpper rows, f)])
                          _
                            -> (s,[])
                    _
                      -> (s, [])
