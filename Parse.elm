module Parse where

import SpreadsheetTypes exposing (..)

parse: String -> Result String CellContent
parse s = Ok (Value (Number 42))
--TODO
